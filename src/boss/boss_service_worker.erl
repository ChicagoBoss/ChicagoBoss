%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@monolite>
%%% @copyright (C) 2012, mihawk
%%% @doc
%%%         generic boss_service to handle websocket event {join/close/incoming}
%%% @end
%%% Created : 23 Jul 2012 by mihawk <mihawk@monolite>
%%%-------------------------------------------------------------------
-module(boss_service_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([incoming/6, join/5, close/6, broadcast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {handler, internal}).
%% TODO: Fix specs (They are real bugs I think)
-spec broadcast(_,_)		-> 'ok'.
-spec close(_,_,_,_,_,_)	-> 'ok'.
-spec code_change(_,_,_)	-> {'ok',_}.
-spec handle_call(_,_,_)	-> {'reply','ok',_}.
%%-spec handle_cast(_,_)		-> 'ok' | {'noreply',_} | {'noreply',#state{},_} | {'stop',_,#state{}}.
%%-spec handle_info(_,#state{})	-> 'ok' | {'noreply',#state{}} | {'noreply',#state{},_} | {'stop',_,#state{}}.
-spec incoming(_,_,_,_,_,_)	-> 'ok'.
%%-spec init([any(),...])		-> 'ok' | {'ok',#state{handler::atom()}}.
-spec join(_,_,_,_,_)		-> 'ok'.
-spec start_link(atom(),_)	-> 'ignore' | {'error',_} | {'ok',pid()}.
-spec terminate(_,#state{})	-> 'ok'.
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Handler, ServiceUrl) when is_atom(Handler)->
    gen_server:start_link({global, Handler}, ?MODULE, [Handler, ServiceUrl], []).

incoming(Service, ServiceUrl, WebSocketId, Req, SessionId, Msg) ->
    gen_server:cast({global, Service}, {incoming_msg, ServiceUrl, WebSocketId, Req, SessionId, Msg}).

join(Service, ServiceUrl, WebSocketId, Req, SessionId) ->
    gen_server:cast({global, Service}, {join_service, ServiceUrl, WebSocketId, Req, SessionId}).

broadcast(Service, Message) ->
    gen_server:cast({global, Service}, {broadcast, Message}).

close(Reason, Service, ServiceUrl, WebSocketId, Req, SessionId) ->
    gen_server:cast({global, Service}, {terminate_service, Reason, ServiceUrl, WebSocketId, Req, SessionId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Handler, ServiceUrl]) when is_atom(Handler) ->
    % todo option to init
    try apply_function_with_default({Handler, undefined, undefined}, init, [], {ok, undefined}) of
	{ok, Internal} ->
	    boss_websocket_router:register(ServiceUrl, Handler),
	    {ok, #state{handler=Handler, internal=Internal}}
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Boss Service Handler ~p terminating in init/0~n"
	      "   for the reason ~p:~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, erlang:get_stacktrace()])	
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({join_service, ServiceUrl, WebSocketId, Req, SessionId}, State) ->
    #state{handler=Handler, internal=Internal} = State,
    try 
	apply_function_with_default({Handler, Req, SessionId}, handle_join,
                             [ServiceUrl, WebSocketId, Internal], {noreply, Internal}) of
	{noreply, NewInternal} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}};
	{noreply, NewInternal, Timeout} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}, Timeout};

	{stop, InternalReason, NewInternal} ->
	    {stop, InternalReason, #state{handler=Handler, internal=NewInternal}}

	catch Class:Reason ->
		error_logger:error_msg(
		  "** Boss Service Handler ~p terminating in join/0~n"
		  "   for the reason ~p:~p~n"
  		  "ServiceUrl: ~p~n"
  		  "WebSocketId: ~p~n"
  		  "SessionId  : ~p~n"
  		  "State    : ~p~n"
		  "** Stacktrace: ~p~n~n",
		  [Handler, Class, Reason, ServiceUrl, WebSocketId,
		   SessionId, Internal, erlang:get_stacktrace()])
	end;

handle_cast({terminate_service, Reason, ServiceUrl, WebSocketId, Req, SessionId}, State) ->   
    #state{handler=Handler, internal=Internal} = State,    
    try apply_function_with_default({Handler, Req, SessionId}, handle_close,
                                    [Reason, ServiceUrl, WebSocketId, Internal], {noreply, Internal}) of
	{noreply, NewInternal} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}};
	{noreply, NewInternal, Timeout} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}, Timeout};
	
	{stop, InternalReason, NewInternal} ->
	    {stop, InternalReason, #state{handler=Handler, internal=NewInternal}}
		
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Handler ~p terminating in init/0~n"
	      "   for the reason ~p:~p~n"
	      "ServiceUrl: ~p~n"
	      "WebSocketId: ~p~n"
	      "SessionId  : ~p~n"
	      "State    : ~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, ServiceUrl, WebSocketId,
	       SessionId, Internal, erlang:get_stacktrace()])	
    end;

handle_cast({incoming_msg, ServiceUrl, WebSocketId, Req, SessionId, Message}, State) ->
    #state{handler=Handler, internal=Internal} = State,
    try apply_function_with_default({Handler, Req, SessionId}, handle_incoming, 
                                    [ServiceUrl, WebSocketId, Message, Internal],
                                   {noreply, Internal}) of
	{noreply, NewInternal} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}};
	{noreply, NewInternal, Timeout} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}, Timeout};
	{stop, _Reason, NewInternal} ->
	    {stop, _Reason, #state{handler=Handler, internal=NewInternal}}		
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Boss Service Handler ~p terminating in handle_incoming/4~n"
	      "   for the reason ~p:~p~n"
	      "ServiceUrl: ~p~n"
	      "WebSocketId: ~p~n"
	      "SessionId  : ~p~n"
	      "Message    : ~p~n"
	      "State    : ~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, ServiceUrl, WebSocketId,
	       SessionId, Message, Internal, erlang:get_stacktrace()])	
    end;

handle_cast({broadcast, Message}, State) ->
    #state{handler=Handler, internal=Internal} = State,
    try apply_function_with_default({Handler, undefined, undefined}, handle_broadcast,
                                    [Message, Internal], {noreply, Internal}) of
	{noreply, NewInternal} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}};
	{noreply, NewInternal, Timeout} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}, Timeout};
	{stop, _Reason, NewInternal} ->
	    {stop, _Reason, #state{handler=Handler, internal=NewInternal}}
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Boss Service Handler ~p terminating in broadcast~n"
	      "   for the reason ~p:~p~n"
	      "Message    : ~p~n"
	      "State    : ~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, Message, Internal, erlang:get_stacktrace()])
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    #state{handler=Handler, internal=Internal} = State,
    try apply_function_with_default({Handler, undefined, undefined}, handle_info, 
                                    [_Info, Internal], {noreply, Internal}) of
	{noreply, NewInternal} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}};
	{noreply, NewInternal, Timeout} ->
	    {noreply, #state{handler=Handler, internal=NewInternal}, Timeout};
	{stop, InternalReason, NewInternal} ->
	    {stop, InternalReason, #state{handler=Handler, internal=NewInternal}}
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Handler ~p terminating in handle_info/2~n"
	      "   for the reason ~p:~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, erlang:get_stacktrace()])	
    end.

%% handle_info(_Info, State) ->
%%      {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    #state{handler=Handler, internal=Internal} = _State,
    try apply_function_with_default({Handler, undefined, undefined}, terminate,
                                    [_Reason, Internal], ok) of
	ok ->
	    ok
    catch Class:Reason ->
	    error_logger:error_msg(
	      "** Boss Service Handler ~p terminating in handle_info/0~n"
	      "   for the reason ~p:~p~n"
	      "** Stacktrace: ~p~n~n",
	      [Handler, Class, Reason, erlang:get_stacktrace()])	
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_function_with_default(Module, Function, Args, Default) when is_tuple(Module) ->
    ModuleAtom = element(1, Module),
    case proplists:get_value(Function, ModuleAtom:module_info(exports)) of
        N when N =:= 1 + length(Args) ->
            erlang:apply(Module, Function, Args);
        _ ->
            Default
    end.
