%% vim: ts=4 sw=4 et

-module(nitrogen_cowboy).

-export([init/3, handle/2, terminate/2]).

-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),
    Body = proplists:get_value(body, Opts, "http_handler"),
    {ok, Req, #state{headers=Headers, body=Body}}.


handle(Req,_Opts) ->
    {ok, DocRoot} = {ok, "./priv/static"},
    io:format("BOOOOOOOOOOOOOO"),
%    RequestBridge = simple_bridge:make_request(cowboy_request_bridge,
%                                               {Req, DocRoot}),

    %% Becaue Cowboy usese the same "Req" record, we can pass the 
    %% previously made RequestBridge to make_response, and it'll
    %% parse out the relevant bits to keep both parts (request and
    %% response) using the same "Req"
%    ResponseBridge = simple_bridge:make_response(cowboy_response_bridge,
%                                                 RequestBridge),

    %% Establishes the context with the Request and Response Bridges
%    nitrogen:init_request(RequestBridge, ResponseBridge),
    io:format("HEREEEEEEEEEEEEEEEEEE"),
    NewReq = boss_web_controller:handle_request(Req, cowboy_request_bridge, cowboy_response_bridge),

    %% This will be returned back to cowboy
    {ok, NewReq, _Opts}.

terminate(_Req, _State) ->
    ok.

