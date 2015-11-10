%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_simple_bridge_handler).
-behaviour(simple_bridge_handler).
-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).

-record(state, {websocket_id, session_id, service_url}).


run(Bridge) ->
    try
         Bridge2 = boss_web_controller_handle_request:handle_request(Bridge, boss_router),
        Bridge2:build_response()
    catch E:C ->
        _ = lager:error("~p:~p: ~p",[E, C, erlang:get_stacktrace()]),
        exit("Error building response")
    end.

ws_init(Bridge) ->
    SessionKey = boss_env:get_env(session_key, "_boss_session"),
    ServiceUrl = list_to_binary(Bridge:path()),
    SessionId  = Bridge:cookie(list_to_binary(SessionKey)),
    WebsocketId = self(),
    State = #state{websocket_id=WebsocketId,
          session_id=SessionId,
          service_url=ServiceUrl},
    boss_websocket_router:join(ServiceUrl, WebsocketId, Bridge, SessionId),
    {ok, State}.

ws_message({text, Data}, Bridge, State) ->
    #state{websocket_id=WebsocketId,
       session_id=SessionId,
       service_url=ServiceUrl } = State,
    Response = case boss_websocket_router:incoming(ServiceUrl, WebsocketId, Bridge, SessionId, Data) of
        ok ->
            noreply;
        {ok, NewState} ->
            {noreply, NewState};
        Res ->
            Res
    end,
    Response;
ws_message({binary, Data}, Bridge, State) ->
    #state{websocket_id=WebsocketId,
       session_id=SessionId,
       service_url=ServiceUrl } = State,
    Response = case boss_websocket_router:incoming(ServiceUrl, WebsocketId, Bridge, SessionId, Data) of
        ok ->
            noreply;
        {ok, NewState} ->
            {noreply, NewState};
        Res ->
            Res
    end,
    Response.

ws_info(Data, _Bridge, _State) ->
    %lager:info("ws_info called with data ~p", [Data]),
    Reply = Data,
    {reply, Reply}.

ws_terminate(Reason, Bridge, State) ->
    #state{websocket_id=WebsocketId,
       session_id=SessionId,
       service_url=ServiceUrl } = State,
    boss_websocket_router:close(Reason, ServiceUrl, WebsocketId, Bridge, SessionId),
    ok.
