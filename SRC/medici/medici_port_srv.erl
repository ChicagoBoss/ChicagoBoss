%%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% File:      medici_port_srv.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @private
%%% An Erlang port server that manages a Tyrant server.
%%% @end
%%%-------------------------------------------------------------------
-module(medici_port_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("medici.hrl").

-record(state, {port=nil, 
		options=[],
		pid=0,
	        log_match,
	        pid_match}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok, Pid} | {error, term()}
%%
%% @private Start the Tyrant port server
start_link() ->
    start_link([]).

start_link(MediciOpts) ->
    ServerOpts = proplists:get_value(run_server, MediciOpts, []),
    case proplists:get_value(server_name, ServerOpts) of
	undefined ->
	    gen_server:start_link({local, ?PORT_SRV_NAME}, ?MODULE, [], []);
	ServerName ->
	    gen_server:start_link({local, ServerName}, ?MODULE, [], [])
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init(MediciOpts) ->
    {ok, LogMatch} = re:compile(?LOG_REGEXP),
    {ok, PidMatch} = re:compile(?PID_REGEXP),
    ServerOpts = proplists:get_value(run_server, MediciOpts, []),
    process_flag(trap_exit, true),
    start_server(ServerOpts, #state{log_match=LogMatch,
				    pid_match=PidMatch}).


handle_call({get_info}, _From, State) ->
    {reply, {State#state.options, State#state.pid}, State};
handle_call({optimize, TuningOpts}, _From, State) ->
    % make optimize call
    % update tuning options in application environment

    % update tuning options in State
    case lists:keysearch(tuning_opts, 1, State#state.options) of
	false ->
	    NewState = State#state{options=State#state.options ++ {tuning_opts, TuningOpts}};
	_ ->
	    NewState = State#state{options=lists:keyreplace(tuning_opts, 1, 
							    State#state.options, 
							    {tuning_opts, TuningOpts})
				  }
    end,
    {reply, ok, NewState};
handle_call({restart, ServerOpts}, _From, State) ->
    case restart_server(ServerOpts, State) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	ErrorMessage ->
	    {reply, ErrorMessage, State}
    end;
handle_call({restart}, _From, State) ->
    case restart_server([], State) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	ErrorMessage ->
	    {reply, ErrorMessage, State}
    end;    
handle_call({stop}, _From, State) ->
    {stop, asked_to_stop, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
    {stop, {port_terminated, Reason},State#state{port=nil, pid=0}};
handle_info({Port, closed}, #state{port=Port} = State) ->
    {stop, {port_terminated, returned_close_msg},State#state{port=nil, pid=0}};
handle_info({Port, {data, {eol, StdOutMsg}}}, #state{port=Port} = State) ->
    parse_log_message(binary_to_list(StdOutMsg), State);
handle_info(_Info, State) ->
    ?DEBUG_LOG("Tyrant port server received unrecognized info message: ~p~n", [_Info]),
    {noreply, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, State) ->
    kill_server(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_server(StartOpts, State) ->
    TyrantBin = proplists:get_value(tyrant_bin, StartOpts, ?TYRANT_BIN),
    case proplists:get_value(data_file, StartOpts, ?DATA_FILE) of
	"*" ->
	    DataFileBase = "\"*\"";
	"+" ->
	    DataFileBase = "\"*\"";
	OtherFile ->
	    DataFileBase = OtherFile
    end,
    TuningOpts = proplists:get_value(tuning_opts, StartOpts, ?TUNING_OPTS),
    case TuningOpts of
	[] ->
	    DataFile = DataFileBase;
	_HasTuningOpts ->
	    DataFile = DataFileBase ++ "#" ++ TuningOpts
    end,
    PortOpts = proplists:get_value(port_opts, StartOpts, ?PORT_OPTS),
    TyrantOpts = proplists:get_value(tyrant_opts, StartOpts, ?TYRANT_OPTS),
    case TyrantOpts of
	[] ->
	    TyrantCommand = TyrantBin ++ " " ++ DataFile;
	_HasTyrantOpts ->
	    TyrantCommand = TyrantBin ++ " " ++ TyrantOpts ++ " " ++ DataFile
    end,
    Port = open_port({spawn, TyrantCommand}, PortOpts),
    {ok, #state{port=Port,
		options=[{tyrant_bin, TyrantBin},
			 {data_file, DataFileBase},
			 {tuning_opts, TuningOpts},
			 {tyrant_opts, TyrantOpts}],
		pid=0,
	        log_match=State#state.log_match,
	        pid_match=State#state.pid_match}}.

restart_server(StartOpts, State) when State#state.pid > 0, State#state.port =/= nil ->
    kill_server(State),
    start_server(StartOpts, State#state{port=nil, pid=0}).

kill_server(State) when State#state.pid > 0, State#state.port =/= nil ->
    %%port_command(State#state.port, <<3:8>>),  % send ^C
    port_close(State#state.port),
    os:cmd("/bin/kill -9 " ++ integer_to_list(State#state.pid));
    %ok;
kill_server(State) when State#state.port =/= nil ->
    port_close(State#state.port);
kill_server(_State) ->
    ok.

parse_log_message(Message, State) when State#state.pid =:= 0 ->
    case re:run(Message, State#state.log_match, [{capture, all_but_first}]) of
	{match, [{MsgStart, _MsgEnd}]} ->
	    {_Head, TyrantMessage} = lists:split(MsgStart, Message),
	    error_logger:info_msg("Tyrant: ~p~n", [TyrantMessage]),
	    case re:run(TyrantMessage, State#state.pid_match, [{capture, all_but_first}]) of
		{match, [{PidStart, _PidEnd}]} ->
			{_PidHead, Pid} = lists:split(PidStart, TyrantMessage),
			{noreply, State#state{pid=list_to_integer(Pid)}};
		_ ->
			{noreply, State}
	    end;
	_ ->
	    ?DEBUG_LOG("Unexpected Tyrant output: ~p~n", [Message]),
	    {noreply, State}
    end;
parse_log_message(Message, State) ->
    case re:run(Message, State#state.log_match, [{capture, all_but_first}]) of
	{match, [{MsgStart, _MsgEnd}]} ->
	    {_Head, TyrantMessage} = lists:split(MsgStart, Message),
	    error_logger:info_msg("Tyrant: ~p~n", [TyrantMessage]),
	    {noreply, State};
	_ ->
	    ?DEBUG_LOG("Unexpected Tyrant output: ~p~n", [Message]),
	    {noreply, State}
    end.
