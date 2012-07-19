%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@monolite>
%%% @copyright (C) 2012, mihawk
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2012 by mihawk <mihawk@monolite>
%%%-------------------------------------------------------------------
-module(boss_service).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

-export([register/2, 
	 unregister/2,
	 join/3,
	 terminate/3,
	 incoming/4
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(boss_consummers, 
	{
	  websocket_id,   % gateway to send message to consummer
          session_id,     % the session id to link and user
	  service_name,   % service name
          created_on      % date of creation
        }).

-record(boss_services, 
	{
	  service_name,   % service name
          service_id,     % the session id 
          created_on      % date of creation
        }).

-record(state, 
	{
	  adapter,   
          conn,     
          nb_consummer,
	  nb_service
        }).
          
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%sync
register(ServiceName, ServiceId) ->
    gen_server:call(?SERVER, {register_service, ServiceName, ServiceId}).

unregister(ServiceName, ServiceId) ->
    gen_server:call(?SERVER, {unregister_service, ServiceName, ServiceId}).

%async
join(ServiceName, WebSocketId, SessionId) ->
    gen_server:cast(?SERVER, {join_service, ServiceName, WebSocketId, SessionId}).

terminate(ServiceName, WebSocketId, SessionId) ->
    gen_server:cast(?SERVER, {terminate_service, ServiceName, WebSocketId, SessionId}).

incoming(ServiceName, WebSocketId, SessionId, Message) ->
    gen_server:cast(?SERVER, {incoming_msg, ServiceName, WebSocketId, SessionId, Message}).
    


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
init([]) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    try
	mnesia:table_info(boss_consummers, type)
    catch
	exit: _ ->
	    mnesia:create_table(boss_consummers, [{attributes, 
						    record_info(fields, boss_consummers)},
					     {type, bag},
					     {disc_copies, [node()]}])
    end,    
    try
	mnesia:table_info(boss_services, type)
    catch
	exit: _ ->
	    mnesia:create_table(boss_services, [{attributes, record_info(fields, boss_services)},
					     {type, bag},
					     {disc_copies, [node()]}])
    end,    
    {ok, #state{nb_consummer = 0, nb_service = 0}}.

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

handle_call({register_service, ServiceName, ServiceId}, _From, State) ->
    Reply = register_service(ServiceName, ServiceId),
    {reply, Reply, State};

handle_call({unregister_service, ServiceName}, _From, State) ->
    Services = get_service(ServiceName),
    Reply = unregister_service(Services),
 %%
 %%  maybe should terminate all consumer who register to this service ???
 %%  (part of programmer, in there loop, my point)
 %%  foreach consummer !{text, "service close"} 
 %%  exit(consummer)
 %% 
    {reply, Reply, State};


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
%% handle_cast({ws_close, WsId, SessionId}, State) ->
%%     Nb = State#state.nb_ws - 1,
%%     Messages = get_message(WsId, SessionId),
%%     %error_logger:info_msg("Msg to delete:~p~n",[Messages]),    
%%     del_message(Messages),
%%     Objs = get_websocket(WsId, SessionId),
%%     %error_logger:info_msg("WebSocket to delete:~p~n",[Objs]),
%%     del_websocket(Objs),
%%     {noreply,  #state{nb_ws=Nb}};
handle_cast({join_service, ServiceName, WebSocketId, SessionId}, State) ->
    case get_service(ServiceName) of
	[{boss_services, _, ServiceId, _}] ->
	    
	    ServiceId ! {join_service, WebSocketId, SessionId};
	Unknow ->
	    error_logger:info_msg("unknow Service (~p)~n", [ServiceName]),
	    Unknow	    
    end,
    register_consummer(ServiceName, WebSocketId, SessionId),
    {noreply, State};

handle_cast({incoming_msg, ServiceName, WebSocketId, SessionId, Msg}, State) ->
    case get_service(ServiceName) of
	[{boss_services, _, ServiceId, _}] ->
	    ServiceId ! {incoming_msg, WebSocketId, SessionId, Msg};
	Unknow ->
	    error_logger:info_msg("unknow Service (~p)~n", [ServiceName]),
	    Unknow	    
    end,
    {noreply, State};

handle_cast({terminate, ServiceName, WebSocketId, SessionId}, State) ->
    case get_service(ServiceName) of
	[{boss_services, _, ServiceId, _}] ->
	    ServiceId ! {terminate_service, WebSocketId, SessionId};
	Unknow ->
	    error_logger:info_msg("unknow Service (~p)~n", [ServiceName]),
	    Unknow	    
    end,

    unregister_consummer([{boss_consummers, 
			   WebSocketId,  
			   SessionId,    
			   ServiceName   
			  }]),
    {noreply, State};

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
    {noreply, State}.

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
    ok.

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

%% Internal functions
register_service(ServiceName, ServiceId) ->
    F = fun() ->
		{_, CreatedOn, _} = erlang:now(),
		mnesia:write(#boss_services{service_id=ServiceId,
					    service_name=ServiceName, 
					    created_on=CreatedOn}) end,
    mnesia:transaction(F).

unregister_service(Services) ->
  F = fun() ->
	  lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Services) end,
  mnesia:transaction(F).


get_service(ServiceName) ->
    F = fun() ->
		Query = qlc:q([M || M <- mnesia:table(boss_services),
				    M#boss_services.service_name =:= ServiceName
			      ]),
		Results = qlc:e(Query),
		Results
	end,
  {atomic, Service} = mnesia:transaction(F),
  Service.


register_consummer(ServiceName, WebsocketId, SessionId) ->
    F = fun() ->
		{_, CreatedOn, _} = erlang:now(),
		mnesia:write(#boss_consummers{
					     service_name=ServiceName, 
					     websocket_id=WebsocketId,
				             session_id=SessionId,
					     created_on=CreatedOn}) end,
    mnesia:transaction(F).

unregister_consummer(Consummers) ->
  F = fun() ->
	  lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Consummers) end,
  mnesia:transaction(F).






