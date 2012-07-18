-module(boss_mochicow_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, loop/1, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, _Opts) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, _Req2} -> {upgrade, protocol, mochicow_upgrade};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

-record(state, {websocket_id, session_id}).

loop(Req) ->
    boss_web_controller:handle_request(Req, mochiweb_request_bridge, mochiweb_response_bridge).

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _Opts) ->
    {SessionId, _Req2}  = cowboy_http_req:cookie(<<"_boss_session">>, Req),
    WebsocketId = self(),
    boss_websocket:ws_open(WebsocketId, SessionId),
    {ok, Req,  #state{websocket_id=WebsocketId, session_id=SessionId}, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    error_logger:info_msg("websocket#message:~p~nWebsocketPid ~p~nState=~p~n", 
			  [Msg, erlang:pid_to_list(self()), State]),    
    #state{websocket_id=WsId, session_id=SsId} = State,
     boss_websocket:put_msg(WsId, SsId, Msg),
    {reply, {text, "copy" }, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, State) ->
   #state{websocket_id=WsId,
	  session_id=SsId
	   }= State,
   boss_websocket:ws_close(WsId, SsId),
    ok.
