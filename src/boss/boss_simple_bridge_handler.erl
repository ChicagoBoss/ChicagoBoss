%% vim: ts=4 sw=4 et
-module(boss_simple_bridge_handler).
-behaviour(simple_bridge_handler).
-export([run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3]).


run(Bridge) ->
    try
	     Bridge2 = boss_web_controller_handle_request:handle_request(Bridge, boss_router),
		Bridge2:build_response()
    catch E:C ->
        error_logger:error_msg("~p:~p: ~p",[E, C, erlang:get_stacktrace()]),
        exit("Error building response")
    end.

ws_init(_Bridge) ->
    %erlang:send_after(1000, self(), "START"),
    ok.

ws_message({text, <<"frag">>}, _State, _Bridge) ->
    Reply = [{text, [Msg," "]} || Msg <- ["A","spoon","full","of","sugar"]],
    {reply, Reply};
ws_message({text, Data}, _State, _Bridge) ->
    %Reply = io_lib:format("~s", [Data]),
    {reply, {text, Data}};
ws_message({binary, Data}, _State, _Bridge) ->
    {reply, {binary, Data}}.

ws_info(Data, _Bridge, _State) ->
    Reply = {text, io_lib:format("~s", [Data])},
    {reply, Reply}.

ws_terminate(_Reason, _Bridge, _State) ->
    ok.
