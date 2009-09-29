%%%-------------------------------------------------------------------
%%% File    : medici_controller.erl
%%% Author  : Jim McCoy <>
%%% Description : A Medici controller that deals with native term storage.
%%%
%%% Created :  5 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_native_controller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_NAME, medici).

-record(state, {clients=[], term_cache}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    {ok, MediciOpts} = application:get_env(options),
    MyName = proplists:get_value(controller, MediciOpts, ?DEFAULT_NAME),
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
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) when length(State#state.clients) =:= 0 ->
    error_logger:error_msg("Request received by controller but no clients available~n"),
    {reply, {error, no_connection_to_server}, State};
handle_call({get, TermKey}, From, State) ->
    dispatch_request({From, get, term_to_binary(TermKey)}, State);
handle_call({out, TermKey}, From, State) ->
    dispatch_request({From, out, term_to_binary(TermKey)}, State);
handle_call({put, TermKey, TermValue}, From, State) ->
    dispatch_request({From, put, term_to_binary(TermKey), term_to_binary(TermValue)}, State);
handle_call({putkeep, TermKey, TermValue}, From, State) ->
    dispatch_request({From, putkeep, term_to_binary(TermKey), term_to_binary(TermValue)}, State);
handle_call({putnr, TermKey, TermValue}, From, State) ->
    dispatch_request({From, putnr, term_to_binary(TermKey), term_to_binary(TermValue)}, State);
handle_call({mget, TermKeyList}, From, State) ->
    dispatch_request({From, mget, [term_to_binary(TermKey) || TermKey <- TermKeyList]}, State);
handle_call({vsiz, TermKey}, From, State) ->
    dispatch_request({From, vsiz, term_to_binary(TermKey)}, State);
handle_call({iterinit}, From, State) ->
    dispatch_request({From, iterinit}, State);
handle_call({iternext}, From, State) ->
    dispatch_request({From, iternext}, State);
handle_call({sync}, From, State) ->
    dispatch_request({From, sync}, State);
handle_call({vanish}, From, State) ->
    dispatch_request({From, vanish}, State);
handle_call({rnum}, From, State) ->
    dispatch_request({From, rnum}, State);
handle_call({size}, From, State) ->
    dispatch_request({From, size}, State);
handle_call({stat}, From, State) ->
    dispatch_request({From, stat}, State);
handle_call({copy, CopyPath}, From, State) ->
    dispatch_request({From, copy, CopyPath}, State);
handle_call({restore, PathName, TimeStamp}, From, State) ->
    dispatch_request({From, restore, PathName, TimeStamp}, State);
handle_call({setmst, HostName, Port}, From, State) ->
    dispatch_request({From, setmst, HostName, Port}, State);
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unknown request received by controller:~n~p~n", [Request]),
    {reply, {error, invalid_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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

handle_info({retry, Pid, _OldReply, OldRequest}, State) when length(State#state.clients) > 1 ->
    dispatch_request(OldRequest, State#state{clients=lists:delete(Pid, State#state.clients)});

handle_info({retry, Pid, OldReply, OldRequest}, State) ->
    gen_server:reply(element(1, OldRequest), OldReply),
    {noreply, State#state{clients=lists:delete(Pid, State#state.clients)}}.


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
    
