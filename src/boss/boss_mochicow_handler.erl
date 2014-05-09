-module(boss_mochicow_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, loop/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	 websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, _Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
	{undefined, _Req2} -> {upgrade, protocol, mochicow_upgrade};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
	{<<"Websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket}
    end.

-record(state, {websocket_id, session_id, service_url}).

loop(Req, RouterAdapter) ->
     boss_web_controller_handle_request:handle_request(Req, 
						      mochiweb_request_bridge, 
						      mochiweb_response_bridge,
                              RouterAdapter
                              ).

terminate(_Req, _State) ->
    ok.

%% TODO check this function
websocket_init(_Any, Req, _Opts) ->
    SessionKey = boss_env:get_env(session_key, "_boss_session"),
    {ServiceUrl, _Req1} = cowboy_req:path(Req),
    {SessionId, _Req2}  = cowboy_req:cookie(list_to_binary(SessionKey), Req),
    WebsocketId = self(),    
    State= #state{websocket_id=WebsocketId, 
		  session_id=SessionId,
		  service_url=ServiceUrl},
    boss_websocket_router:join(ServiceUrl, WebsocketId, Req, SessionId),
    case boss_env:get_env(websocket_timeout, undefined) of
        undefined ->
            {ok, Req, State, hibernate};
        Timeout ->
            {ok, Req, State, Timeout, hibernate}
    end.

websocket_handle({text, Msg}, Req, State) ->
    #state{websocket_id=WebsocketId, 
	   session_id=SessionId, 
	   service_url=ServiceUrl } = State,
    boss_websocket_router:incoming(ServiceUrl, WebsocketId, Req, SessionId, Msg),
    {ok, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, Req, State) ->
    #state{websocket_id=WebsocketId, 
	   session_id=SessionId, 
	   service_url=ServiceUrl } = State,
    boss_websocket_router:close(Reason, ServiceUrl, WebsocketId, Req, SessionId),
    ok.
