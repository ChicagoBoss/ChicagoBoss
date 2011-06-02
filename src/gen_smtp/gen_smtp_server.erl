%%% Copyright 2009 Andrew Thompson <andrew@hijacked.us>. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimer.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT ``AS IS'' AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
%%% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% @doc A non-blocking tcp listener for SMTP connections. Based on the tcp_listener module by Serge
%% Aleynikov [http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles]

-module(gen_smtp_server).
-behaviour(gen_server).

-define(PORT, 2525).

%% External API
-export([start_link/3, start_link/2, start_link/1,
    start/3, start/2, start/1,
    stop/1, sessions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		code_change/3]).


-record(listener, {
		hostname :: list(),
		port :: port(),
		sessionoptions = [] :: [tuple()],
		socket :: port() | any(),
		listenoptions = [] :: [tuple()]
		}).
-type(listener() :: #listener{}).

-record(state, {
		listeners :: [listener()],  % Listening sockets (tcp or ssl)
		module :: atom(),
		sessions = [] :: [pid()]
		}).

-type(options() :: [{'domain', string()} | {'address', {pos_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}} |
		{'port', pos_integer()} | {'protocol', 'tcp' | 'ssl'} | {'sessionoptions', [any()]}]).

%% @doc Start the listener as a registered process with callback module `Module' on with options `Options' linked to the calling process.
-spec(start_link/3 :: (ServerName :: {'local', atom()} | {'global', any()}, Module :: atom(), Options :: [options()]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(ServerName, Module, Options) when is_list(Options) ->
	gen_server:start_link(ServerName, ?MODULE, [Module, Options], []).

%% @doc Start the listener with callback module `Module' on with options `Options' linked to the calling process.
-spec(start_link/2 :: (Module :: atom(), Options :: [options()]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Module, Options) when is_list(Options) ->
	gen_server:start_link(?MODULE, [Module, Options], []).

%% @doc Start the listener with callback module `Module' with default options linked to the calling process.
-spec(start_link/1 :: (Module :: atom()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start_link(Module) ->
	start_link(Module, [[]]).

%% @doc Start the listener as a registered process with callback module `Module' with options `Options' linked to no process.
-spec(start/3 :: (ServerName :: {'local', atom()} | {'global', any()}, Module :: atom(), Options :: [options()]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(ServerName, Module, Options) when is_list(Options) ->
	gen_server:start(ServerName, ?MODULE, [Module, Options], []).

%% @doc Start the listener with callback module `Module' with options `Options' linked to no process.
-spec(start/2 :: (Module :: atom(), Options :: [options()]) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Module, Options) when is_list(Options) ->
	gen_server:start(?MODULE, [Module, Options], []).

%% @doc Start the listener with callback module `Module' with default options linked to no process.
-spec(start/1 :: (Module :: atom()) -> {'ok', pid()} | 'ignore' | {'error', any()}).
start(Module) ->
	start(Module, [[]]).

%% @doc Stop the listener pid() `Pid' with reason `normal'.
-spec(stop/1 :: (Pid :: pid()) -> 'ok').
stop(Pid) ->
	gen_server:call(Pid, stop).

%% @doc Return the list of active SMTP session pids.
-spec sessions(Pid :: pid()) -> [pid()].
sessions(Pid) ->
	gen_server:call(Pid, sessions).

%% @doc
%% The gen_smtp_server is given a list of tcp listener configurations.
%% You'll typically only want to listen on one port so your options
%% will be a single-item list containing a proplist. e.g.:
%%
%% <pre>  [[{port,25},{protocol,tcp},{domain,"myserver.com"},{address,,{0,0,0,0}}]]</pre>
%%
%% By providing additional configurations the server will listen on multiple
%% ports over either tcp or ssl on any given port. e.g.:
%% <pre>
%%  [
%%    [{port,25},{protocol,tcp},{domain,"myserver.com"},{address,{0,0,0,0}}],
%%    [{port,465},{protocol,ssl},{domain,"secure.myserver.com"},{address,{0.0.0.0}}],
%%    [{port, 25},{family, inet6},{domain,"ipv6.myserver.com"},{address,"::"}]
%%  ]
%% </pre>
%% Note that the default port to listen on is `2525' because the regular SMTP
%% ports are privileged and only bindable by root. The default protocol is
%% `tcp', the default listen address is `0.0.0.0' and the default address family
%% is `inet'. Anything passed in the `sessionoptions' option, is passed through
%% to `gen_server_smtp_session'.
%% @see gen_smtp_server_session
-spec(init/1 :: (Args :: list()) -> {'ok', #state{}} | {'stop', any()}).
init([Module, Configurations]) ->
	DefaultConfig = [{domain, smtp_util:guess_FQDN()}, {address, {0,0,0,0}},
		{port, ?PORT}, {protocol, tcp}, {family, inet}],
	try
		case Configurations of
			[FirstConfig|_] when is_list(FirstConfig) -> ok;
			_ -> exit({init,"Please start gen_smtp_server with an options argument formatted as a list of proplists"})
		end,
		Listeners = [
			begin
					NewConfig = lists:ukeymerge(1, lists:sort(Config), lists:sort(DefaultConfig)),
					Port = proplists:get_value(port, NewConfig),
					IP = proplists:get_value(address, NewConfig),
					Family = proplists:get_value(family, NewConfig),
					Hostname = proplists:get_value(domain, NewConfig),
					Protocol = proplists:get_value(protocol, NewConfig),
					SessionOptions = proplists:get_value(sessionoptions, NewConfig, []),
					error_logger:info_msg("~p starting at ~p~n", [?MODULE, node()]),
					error_logger:info_msg("listening on ~p:~p via ~p~n", [IP, Port, Protocol]),
					process_flag(trap_exit, true),
					ListenOptions = [binary, {ip, IP}, Family],
					case socket:listen(Protocol, Port, ListenOptions) of
						{ok, ListenSocket} -> %%Create first accepting process
							socket:begin_inet_async(ListenSocket),
							#listener{port = socket:extract_port_from_socket(ListenSocket),
								hostname = Hostname, sessionoptions = SessionOptions,
								socket = ListenSocket, listenoptions = ListenOptions};
						{error, Reason} ->
							exit({init, Reason})
					end
			end || Config <- Configurations],
		{ok, #state{listeners = Listeners, module = Module}}
	catch exit:Why ->
		{stop, Why}
  end.

%% @hidden
-spec handle_call(Message :: any(), From :: {pid(), reference()}, State :: #state{}) -> {'stop', 'normal', 'ok', #state{}} | {'reply', any(), #state{}}.
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};

handle_call(sessions, _From, State) ->
	{reply, State#state.sessions, State};

handle_call(Request, _From, State) ->
	{reply, {unknown_call, Request}, State}.

%% @hidden
-spec handle_cast(Message :: any(), State :: #state{}) -> {'noreply', #state{}}.
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @hidden
-spec handle_info(Message :: any(), State :: #state{}) -> {'noreply', #state{}} | {'stop', any(), #state{}}.
handle_info({inet_async, ListenPort,_, {ok, ClientAcceptSocket}},
	#state{module = Module, listeners = Listeners, sessions = CurSessions} = State) ->
	try
		% find this ListenPort in our listeners.
		[Listener] = lists:flatten([case L#listener.port of
					ListenPort -> L;
					_ -> []
				end || L <- Listeners]),
		{ok, ClientSocket} = socket:handle_inet_async(Listener#listener.socket, ClientAcceptSocket, Listener#listener.listenoptions),
		%% New client connected
		% io:format("new client connection.~n", []),
		Sessions = case gen_smtp_server_session:start(ClientSocket, Module, [{hostname, Listener#listener.hostname}, {sessioncount, length(CurSessions) + 1} | Listener#listener.sessionoptions]) of
			{ok, Pid} ->
				link(Pid),
				socket:controlling_process(ClientSocket, Pid),
				CurSessions ++[Pid];
			_Other ->
				CurSessions
		end,
		{noreply, State#state{sessions = Sessions}}
	catch _:Error ->
		error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
		{noreply, State}
	end;
handle_info({'EXIT', From, Reason}, State) ->
	case lists:member(From, State#state.sessions) of
		true ->
			{noreply, State#state{sessions = lists:delete(From, State#state.sessions)}};
		false ->
			io:format("process ~p exited with reason ~p~n", [From, Reason]),
			{noreply, State}
	end;
handle_info({inet_async, ListenSocket, _, {error, econnaborted}}, State) ->
	io:format("Client terminated connection with econnaborted~n"),
	socket:begin_inet_async(ListenSocket),
	{noreply, State};
handle_info({inet_async, _ListenSocket,_, Error}, State) ->
	error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
	{stop, Error, State};
handle_info(_Info, State) ->
	{noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> 'ok'.
terminate(Reason, State) ->
	io:format("Terminating due to ~p~n", [Reason]),
	lists:foreach(fun(#listener{socket=S}) -> catch socket:close(S) end, State#state.listeners),
	ok.

%% @hidden
-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {'ok', #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
