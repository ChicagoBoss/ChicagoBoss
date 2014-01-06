%%%-------------------------------------------------------------------
%%% @author mihawk <mihawk@monolite>
%%% @copyright (C) 2012, mihawk
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2012 by mihawk <mihawk@monolite>
%%%-------------------------------------------------------------------
-module(boss_websocket_router).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

-export([register/2, 
	 unregister/2,
	 join/4,
	 close/5,
	 incoming/5,
	 service/1,
	 services/0,
	 consumers/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-record(state, {consumers  ::dict(), 
		services   ::dict(), 
		nb_consumer}).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
-spec register(_,_) -> any().
-spec unregister(_,_) -> any().
-spec service(_) -> any().
-spec services() -> any().
-spec consumers() -> any().
-spec join(_,_,_,_) -> 'ok'.
-spec close(_,_,_,_,_) -> 'ok'.
-spec incoming(_,_,_,_,_) -> 'ok'.
-spec init([]) -> {'ok',#state{consumers::dict(),services::dict(),nb_consumer::0}}.
-spec handle_call(_,_,#state{}) -> {'reply','ok' | [any()] | {'error','service_notfound'} | {'ok',_},#state{}}.
-spec handle_cast(_,#state{}) -> {'noreply',#state{}}.
-spec handle_info(_,#state{}) -> {'noreply',#state{}}.
-spec terminate(_,_) -> 'ok'.
-spec code_change(_,_,_) -> {'ok',_}.


%% -record(boss_consumers, 
%% 	{
%% 	  websocket_id,   % gateway to send message to consummer
%%           session_id,     % the session id to link and user
%% 	  service_name,   % service name
%%           created_on      % date of creation
%%         }).

%% -record(boss_services, 
%% 	{
%% 	  service_name,   % service name
%%           service_id,     % the session id 
%%           created_on      % date of creation
%%         }).


          

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
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%sync
register(ServiceUrl, ServiceId) ->
    gen_server:call({global, ?SERVER}, {register_service, ServiceUrl, ServiceId}).

unregister(ServiceUrl, ServiceId) ->
    gen_server:call({global, ?SERVER}, {unregister_service, ServiceUrl, ServiceId}).

service(ServiceUrl) ->
    gen_server:call({global, ?SERVER}, {get_service, ServiceUrl}).

services() ->
    gen_server:call({global, ?SERVER}, {get_all_service}).

consumers() ->
    gen_server:call({global, ?SERVER}, {get_consumers}).
    

    

%async
join(ServiceUrl, WebSocketId, Req, SessionId) ->
    gen_server:cast({global, ?SERVER}, {join_service, ServiceUrl, WebSocketId, Req, SessionId}).

close(Reason, ServiceUrl, WebSocketId, Req, SessionId) ->
    gen_server:cast({global, ?SERVER}, {terminate_service, Reason, ServiceUrl, WebSocketId, Req, SessionId}).

incoming(ServiceUrl, WebSocketId, Req, SessionId, Message) ->
    gen_server:cast({global, ?SERVER}, {incoming_msg, ServiceUrl, WebSocketId, Req, SessionId, Message}).

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
    {ok, #state{consumers=dict:new(), services=dict:new(), nb_consumer=0}}.

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

handle_call({get_service, ServiceUrl}, _From, State) ->
    #state{services=Services} = State,    
    Reply = case dict:find(ServiceUrl, Services) of
                {ok, Val} ->
                    {ok, Val};
                error ->
                    {error, service_notfound}
            end,
    {reply, Reply, State};

handle_call({get_all_service}, _From, State) ->
    #state{services=Services} = State,    
    Reply = dict:fetch_keys(Services),
    {reply, Reply, State};

handle_call({get_consumers}, _From, State) ->
    #state{consumers=Consumers} = State,    
    Reply = dict:fetch_keys(Consumers),
    {reply, Reply, State};

handle_call({register_service, ServiceUrl, ServiceId}, _From, State) ->
    #state{consumers=ConsumersStore, services=ServicesStore, nb_consumer=NbConsumer} = State,    
    NewServicesStore = dict:store(ServiceUrl, ServiceId, ServicesStore),
    NewState = #state{consumers=ConsumersStore, services=NewServicesStore,nb_consumer=NbConsumer},
    {reply, ok, NewState};

handle_call({unregister_service, ServiceUrl}, _From, State) ->
    #state{consumers=Consumers, services=ServicesStore, nb_consumer=NbConsumer} = State,    
    NewServices = dict:erase(ServiceUrl, ServicesStore),
    {reply, ok, #state{consumers=Consumers, services=NewServices, nb_consumer=NbConsumer}};

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
    #state{consumers=Consumers, services=Services, nb_consumer=NbConsumer} = State,    
    case dict:find(ServiceUrl, Services) of
        {ok, ServiceId} ->
            boss_service_worker:join(ServiceId, binary_to_list(ServiceUrl), WebSocketId, Req, SessionId),
            NewConsumers = dict:store(WebSocketId, [ServiceId, SessionId], Consumers),
            NewState = #state{consumers=NewConsumers, services=Services,nb_consumer=NbConsumer+1},
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_cast({incoming_msg, ServiceUrl, WebSocketId, Req, SessionId, Msg}, State) ->
    #state{services=Services} = State,    
    case dict:find(ServiceUrl, Services) of
        {ok, ServiceId} ->
            boss_service_worker:incoming(ServiceId, binary_to_list(ServiceUrl), WebSocketId, Req, SessionId, Msg),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast({terminate_service, Reason, ServiceUrl, WebSocketId, Req, SessionId}, State) ->
    #state{consumers=Consumers, services=Services, nb_consumer=NbConsumer} = State,    
    case dict:find(ServiceUrl, Services) of
        {ok, ServiceId} ->                
            boss_service_worker:close(Reason, ServiceId, binary_to_list(ServiceUrl), WebSocketId, Req, SessionId),
            NewConsumers = dict:erase(WebSocketId, Consumers),
            NewState = #state{consumers=NewConsumers, services=Services, nb_consumer=NbConsumer-1},
            {noreply, NewState};
        error ->
            {noreply, State}
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

