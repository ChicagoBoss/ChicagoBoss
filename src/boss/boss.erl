%% @author Evan Miller <emmiller@gmail.com>
%% @copyright 2009 author.

%% @doc TEMPLATE.

-module(boss).
-author('Evan Miller <emmiller@gmail.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the boss server.
start() ->
    is_compatible(erlang:system_info(otp_release)),
    ensure_started(crypto),
    ensure_started(mimetypes),
    application:start(boss).

%% @spec stop() -> ok
%% @doc Stop the boss server.
stop() ->
    Res = application:stop(boss),    
    application:stop(mimetypes),    
    application:stop(crypto),
    Res.

is_compatible("R16B03") ->
    lager:emergency("Chicago Boss is not compatible with R16B03"),
    erlang:halt(1, "Chicago Boss is not compatible with R16B03");
is_compatible(_) ->
    ok.
