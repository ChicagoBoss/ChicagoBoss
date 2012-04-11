%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(boss_web_cowboy_sup).
-behaviour(supervisor).
-export([
    start_link/1,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Config) ->
    %io:format("CONFIG: ~p~n", [Config]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Config]) ->

    %% Start Cowboy...
    application:start(cowboy),
    BindAddress = proplists:get_value(ip, Config),
    {ok, Ip} = inet_parse:address(BindAddress),
    Port = proplists:get_value(port, Config),
    
    %% Static file serving will be handled by CB because we need to accomodate Mochiweb
    %% DocRoot =  "./priv/static",
    %% StaticPaths = ["static/"],
    %% DocRootBin = list_to_binary(DocRoot),

    %% StaticDispatches = lists:map(fun(Dir) ->
    %%    Path = reformat_path(Dir),
    %%    Handler = cowboy_http_static,
    %%    Opts = [
    %%        {directory, filename:join([DocRootBin | Path])},
    %%        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
    %%    ],
    %%    {Path ++ ['...'],Handler,Opts}
    %% end,StaticPaths),

    %% HandlerModule will end up calling HandlerModule:handle(Req,HandlerOpts)
    HandlerModule = boss_web_cowboy,
    HandlerOpts = [],

    %% Start Cowboy...
    %% NOTE: According to Loic, there's no way to pass the buck back to cowboy 
    %% to handle static dispatch files so we want to make sure that any large 
    %% files get caught in general by cowboy and are never passed to the nitrogen
    %% handler at all. In general, your best bet is to include the directory in
    %% the static_paths section of cowboy.config
    Dispatch = [
        %% ChicagoBoss will handle everything that's not handled in the StaticDispatches
        %% {'_', StaticDispatches ++ [{'_',HandlerModule , HandlerOpts}]}
        %% But instead, we make ChicagoBoss handle everything
        {'_', [{'_',HandlerModule , HandlerOpts}]}
    ],
    HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{ip, Ip}, {port, Port}],
                          cowboy_http_protocol, HttpOpts),


    {ok, { {one_for_one, 0, 10}, []} }.

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

