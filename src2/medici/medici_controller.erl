%%%-------------------------------------------------------------------
%%% File    : medici_controller.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  5 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("medici.hrl").

-record(state, {clients=[], auto_sync=nil, auto_tune=nil, auto_copy=nil}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(MediciOpts) ->
    MyName = proplists:get_value(controller_name, MediciOpts, ?CONTROLLER_NAME),
    gen_server:start_link({local, MyName}, ?MODULE, MediciOpts, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_ClientProps) ->
    timer:start(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) when length(State#state.clients) =:= 0 ->
    ?DEBUG_LOG("Request received by controller but no clients available~n", []),
    {reply, {error, no_connection_to_server}, State};
handle_call({task, TaskType, TaskParams}, _From, State) ->
    NewState = task(State, TaskType, TaskParams),
    {reply, ok, NewState};
handle_call({CallFunc}, From, State) ->
    %% Yes, a single atom in a tuple is "bad form", but makes the pattern matching work...
    dispatch_request({From, CallFunc}, State);
handle_call({CallFunc, Arg1}, From, State) ->
    dispatch_request({From, CallFunc, Arg1}, State);
handle_call({CallFunc, Arg1, Arg2}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3, Arg4}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3, Arg4}, State);
handle_call(_Request, _From, State) ->
    ?DEBUG_LOG("Unknown request received by medici controller: ~p", [_Request]),
    {reply, {error, invalid_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({putnr, Key, Value}, State) ->
    dispatch_request({putnr, Key, Value}, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    ?DEBUG_LOG("Unknown cast received by medici controller: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({client_start, Pid}, State) ->
    {noreply, State#state{clients=[Pid | State#state.clients]}};
handle_info({client_end, Pid}, State) ->
    {noreply, State#state{clients=lists:delete(Pid, State#state.clients)}};
%% The retries should either do a hard-kill of the clients it is eliminating
%% from the list, but 
handle_info({retry, Pid, _OldReply, OldRequest}, State) when length(State#state.clients) > 1 ->
    dispatch_request(OldRequest, State#state{clients=lists:delete(Pid, State#state.clients)});
handle_info({retry, Pid, OldReply, OldRequest}, State) ->
    gen_server:reply(element(1, OldRequest), OldReply),
    {noreply, State#state{clients=lists:delete(Pid, State#state.clients)}};
handle_info(_Msg, State) ->
    ?DEBUG_LOG("Unknown info message received by medici controller: ~p", [_Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
dispatch_request(Request, State) when length(State#state.clients) =:= 1 ->
    gen_server:cast(hd(State#state.clients), Request),
    {noreply, State};
dispatch_request(Request, State) ->
    [TgtClient | OtherClients] = State#state.clients,
    gen_server:cast(TgtClient, Request),
    {noreply, State#state{clients=OtherClients++[TgtClient]}}.

task(State, sync, false) when State#state.auto_sync =:= nil->
    State;
task(State, sync, false) ->
    {TRef, _} = State#state.auto_sync,
    {ok, cancel} = timer:cancel(TRef),
    State#state{auto_sync=nil};
task(State, sync, true) when State#state.auto_sync =/= nil ->
    State;
task(State, sync, true) ->
    {OldTRef, OldPeriod} = State#state.auto_sync,
    case OldPeriod =/= ?DEFAULT_SYNC_PERIOD of
	true ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(?DEFAULT_SYNC_PERIOD, {sync});
	_ ->
	    TRef = OldTRef
    end,
    State#state{auto_sync={TRef, ?DEFAULT_SYNC_PERIOD}};
task(State, sync, Period) when is_integer(Period), Period > 0 ->
    case State#state.auto_sync of
	nil ->
	    TRef = timer:send_interval(Period * 1000, {sync}),
	    State#state{auto_sync={TRef, Period * 1000}};
	{OldTRef, _} ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(Period * 1000, {sync}),
	    State#state{auto_sync={TRef, Period * 1000}}
    end;
task(State, sync, Period) when is_integer(Period) ->
    task(State, sync, false);

task(State, tune, false) when State#state.auto_tune =:= nil->
    State;
task(State, tune, false) ->
    {TRef, _} = State#state.auto_tune,
    {ok, cancel} = timer:cancel(TRef),
    State#state{auto_tune=nil};
task(State, tune, true) when State#state.auto_tune =/= nil ->
    State;
task(State, tune, true) ->
    {OldTRef, OldPeriod} = State#state.auto_tune,
    case OldPeriod =/= ?DEFAULT_TUNE_PERIOD of
	true ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(?DEFAULT_TUNE_PERIOD, {tune});
	_ ->
	    TRef = OldTRef
    end,
    State#state{auto_tune={TRef, ?DEFAULT_SYNC_PERIOD}};
task(State, tune, Period) when is_integer(Period), Period > 0 ->
    case State#state.auto_tune of
	nil ->
	    TRef = timer:send_interval(Period * 1000, {tune}),
	    State#state{auto_tune={TRef, Period * 1000}};
	{OldTRef, _} ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(Period * 1000, {tune}),
	    State#state{auto_tune={TRef, Period * 1000}}
    end;
task(State, tune, Period) when is_integer(Period) ->
    task(State, tune, false);

task(State, copy, false) when State#state.auto_copy =:= nil->
    State;
task(State, copy, false) ->
    {TRef, _} = State#state.auto_copy,
    {ok, cancel} = timer:cancel(TRef),
    State#state{auto_copy=nil};
task(State, copy, {Target, Period}) when is_integer(Period), Period > 60 ->
    case State#state.auto_copy of
	nil ->
	    TRef = timer:send_interval(Period * 1000, {copy, Target}),
	    State#state{auto_tune={TRef, {Target, Period * 1000}}};
	{OldTRef, _} ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(Period * 1000, {copy, Target}),
	    State#state{auto_tune={TRef, {Target, Period * 1000}}}
    end;
task(State, tune, Period) when is_integer(Period) ->
    task(State, tune, false).


