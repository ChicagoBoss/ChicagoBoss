%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(nitrogen_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    %% Start Cowboy...
    application:start(cowboy),
    {ok, BindAddress} = {ok, "0.0.0.0"},
    {ok, Port} = {ok, 8080},
    {ok, ServerName} = {ok, nitrogen},
    {ok, DocRoot} = {ok, "./priv/static"},
    {ok, StaticPaths} = {ok, ["js/","images/","css/","nitrogen/"]},
    DocRootBin = list_to_binary(DocRoot),

    io:format("Starting Cowboy Server (~s) on ~s:~p, root: '~s'~n",
              [ServerName, BindAddress, Port, DocRoot]),

    StaticDispatches = lists:map(fun(Dir) ->
        Path = reformat_path(Dir),
        Handler = cowboy_http_static,
        Opts = [
            {directory, filename:join([DocRootBin | Path])},
            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ],
        {Path ++ ['...'],Handler,Opts}
    end,StaticPaths),

    %% HandlerModule will end up calling HandlerModule:handle(Req,HandlerOpts)
    HandlerModule = nitrogen_cowboy,
    HandlerOpts = [],

    %% Start Cowboy...
    %% NOTE: According to Loic, there's no way to pass the buck back to cowboy 
    %% to handle static dispatch files so we want to make sure that any large 
    %% files get caught in general by cowboy and are never passed to the nitrogen
    %% handler at all. In general, your best bet is to include the directory in
    %% the static_paths section of cowboy.config
    Dispatch = [
        %% Nitrogen will handle everything that's not handled in the StaticDispatches
        {'_', StaticDispatches ++ [{'_',HandlerModule , HandlerOpts}]}
    ],
    HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, HttpOpts),

    {ok, { {one_for_one, 5, 10}, []} }.

%% If the path is a string, it's going to get split on / and switched to the binary
%% format that cowboy expects
reformat_path(Path) when is_list(Path) andalso is_integer(hd(Path)) ->
    TokenizedPath = string:tokens(Path,"/"),
    [list_to_binary(Part) || Part <- TokenizedPath];
%% If path is just a binary, let's make it a string and treat it as such
reformat_path(Path) when is_binary(Path) ->
    reformat_path(binary_to_list(Path));
%% If path is a list of binaries, then we assumed it's formatted for cowboy format already
reformat_path(Path) when is_list(Path) andalso is_binary(hd(Path)) ->
    Path.

