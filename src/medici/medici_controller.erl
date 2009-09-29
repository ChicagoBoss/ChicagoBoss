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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_NAME, medici).

-record(state, {clients=[]}).

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
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
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
handle_call({CallFunc}, From, State) ->
    dispatch_request({From, CallFunc}, State);
handle_call({CallFunc, Arg1}, From, State) ->
    dispatch_request({From, CallFunc, Arg1}, State);
handle_call({CallFunc, Arg1, Arg2}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3, Arg4}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3, Arg4}, State);
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
    
