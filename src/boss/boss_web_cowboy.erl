%% vim: ts=4 sw=4 et

-module(boss_web_cowboy).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),
    Body = proplists:get_value(body, Opts, "http_handler"),
    {ok, Req, #state{headers=Headers, body=Body}}.


handle(Req,_Opts) ->

    {ok, NewReq} = boss_web_controller:handle_request(Req, cowboy_request_bridge, cowboy_response_bridge),
    
    {ok, NewReq, _Opts}.

terminate(_Req, _State) ->
    ok.
