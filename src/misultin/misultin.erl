% ==========================================================================================================
% MISULTIN - Main
%
% >-|-|-(°>
% 
% Copyright (C) 2010, Roberto Ostinelli <roberto@ostinelli.net>, Sean Hinde.
% All rights reserved.
%
% Code portions from Sean Hinde have been originally taken under BSD license from Trapexit at the address:
% <http://www.trapexit.org/A_fast_web_server_demonstrating_some_undocumented_Erlang_features>
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%	 following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%	 the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%	 products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(misultin).
-behaviour(gen_server).
-vsn("0.6.0").

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
-export([start_link/1, stop/0, persistent_socket_pid_add/1, persistent_socket_pid_remove/1]).

% macros
-define(SERVER, ?MODULE).

% records
-record(state, {
	% tcp
	listen_socket,
	socket_mode,
	port,
	options,
	acceptor,
	recv_timeout,
	persistent_sock_references = [],
	% misultin
	custom_opts
}).

% includes
-include("../include/misultin.hrl").


% ============================ \/ API ======================================================================

% Function: {ok,Pid} | ignore | {error, Error}
% Description: Starts the server.
start_link(Options) when is_list(Options) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

% Function: -> ok
% Description: Manually stops the server.
stop() ->
	gen_server:cast(?SERVER, stop).

% Function -> ok
% Description: Adds a new persistent socket pid reference to status
persistent_socket_pid_add(WsPid) ->
	gen_server:cast(?SERVER, {add_ws_pid, WsPid}).

% Function -> ok
% Description: Remove a persistent socket pid reference from status
persistent_socket_pid_remove(WsPid) ->
	gen_server:cast(?SERVER, {remove_ws_pid, WsPid}).

% ============================ /\ API ======================================================================


% ============================ \/ GEN_SERVER CALLBACKS =====================================================

% ----------------------------------------------------------------------------------------------------------
% Function: -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
% Description: Initiates the server.
% ----------------------------------------------------------------------------------------------------------
init([Options]) ->
	process_flag(trap_exit, true),
	?LOG_INFO("starting with Pid: ~p", [self()]),
	% test and get options
	OptionProps = [
		% socket
		{ip, {0, 0, 0, 0}, fun check_and_convert_string_to_ip/1, invalid_ip},
		{port, 80, fun is_integer/1, port_not_integer},
		{backlog, 128, fun is_integer/1, backlog_not_integer},
		{recv_timeout, 30*1000, fun is_integer/1, recv_timeout_not_integer},
		{ssl, false, fun check_ssl_options/1, invalid_ssl_options},
		% misultin
		{compress, false, fun is_boolean/1, invalid_compress_option},
		{stream_support, true, fun is_boolean/1, invalid_stream_support_option},
		{loop, {error, undefined_loop}, fun is_function/1, loop_not_function},
		{ws_loop, none, fun is_function/1, ws_loop_not_function}
	],
	OptionsVerified = lists:foldl(fun(OptionName, Acc) -> [get_option(OptionName, Options)|Acc] end, [], OptionProps),
	case proplists:get_value(error, OptionsVerified) of
		undefined ->
			% ok, no error found in options
			% tcp options
			Ip = proplists:get_value(ip, OptionsVerified),
			Port = proplists:get_value(port, OptionsVerified),
			Backlog = proplists:get_value(backlog, OptionsVerified),
			RecvTimeout = proplists:get_value(recv_timeout, OptionsVerified),
			SslOptions0 = proplists:get_value(ssl, OptionsVerified),
			% misultin options
			Compress = proplists:get_value(compress, OptionsVerified),
			StreamSupport = proplists:get_value(stream_support, OptionsVerified),
			Loop = proplists:get_value(loop, OptionsVerified),
			WsLoop = proplists:get_value(ws_loop, OptionsVerified),
			% ipv6 support
			?LOG_DEBUG("ip address is: ~p", [Ip]),
			% set additional options according to socket mode if necessary
			Continue = case SslOptions0 of
				false ->
					% without SSL
					SocketMode = http,
					InetOpt = case Ip of
						{_, _, _, _} ->
							% IPv4
							inet;
						{_, _, _, _, _, _, _, _} ->
							% IPv6
							inet6
					end,
					AdditionalOptions = [InetOpt],
					true;
				_ ->
					% with SSL
					SocketMode = ssl,
					% the only current way to use {active, once} in Ssl is to start the crypto module
					% and set {ssl_imp, new} as SSL option, see
					% <http://www.erlang.org/cgi-bin/ezmlm-cgi?4:mss:50633:201004:fpopocbfkpppecdembbe>
					AdditionalOptions = [{ssl_imp, new}|SslOptions0],
					% start Ssl and crypto applications if necessary, and get outcomes
					AppStartResults = lists:keyfind(error, 1, [start_application(ssl), start_application(crypto)]),
					case AppStartResults of
						false ->
							% all applications started succesfully
							true;
						_ ->
							% error starting application
							{error, AppStartResults}
					end
			end,
			% proceed?
			case Continue of
				true ->
					% set options
					OptionsTcp = [binary, {packet, raw}, {ip, Ip}, {reuseaddr, true}, {active, false}, {backlog, Backlog}|AdditionalOptions],
					% build custom_opts
					CustomOpts = #custom_opts{compress = Compress, stream_support = StreamSupport, loop = Loop, ws_loop = WsLoop},
					% create listening socket and acceptor
					case create_listener_and_acceptor(Port, OptionsTcp, RecvTimeout, SocketMode, CustomOpts) of
						{ok, ListenSocket, AcceptorPid} ->
							?LOG_DEBUG("listening socket and acceptor succesfully started",[]),
							{ok, #state{listen_socket = ListenSocket, socket_mode = SocketMode, port = Port, options = OptionsTcp, acceptor = AcceptorPid, recv_timeout = RecvTimeout, custom_opts = CustomOpts}};
						{error, Reason} ->
							?LOG_ERROR("error starting listener socket: ~p", [Reason]),
							{stop, Reason}
					end;
				Error ->
					{stop, Error}
			end;
		Reason ->
			% error found in options
			{stop, Reason}
	end.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_call(Request, From, State) -> {reply, Reply, State} | {reply, Reply, State, Timeout} |
%									   {noreply, State} | {noreply, State, Timeout} |
%									   {stop, Reason, Reply, State} | {stop, Reason, State}
% Description: Handling call messages.
% ----------------------------------------------------------------------------------------------------------

% handle_call generic fallback
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling cast messages.
% ----------------------------------------------------------------------------------------------------------

% manual shutdown
handle_cast(stop, State) ->
	?LOG_INFO("manual shutdown..", []),
	{stop, normal, State};

% add persistent socket reference to server
handle_cast({add_ws_pid, PsPid}, #state{persistent_sock_references = PersistentSockReferences} = State) ->
	{noreply, State#state{persistent_sock_references = [PsPid|PersistentSockReferences]}};

% remove persistent socket reference from server
handle_cast({remove_ws_pid, PsPid}, #state{persistent_sock_references = PersistentSockReferences} = State) ->
	{noreply, State#state{persistent_sock_references = lists:delete(PsPid, PersistentSockReferences)}};
	
% handle_cast generic fallback (ignore)
handle_cast(_Msg, State) ->
	?LOG_WARNING("received unknown cast message: ~p", [_Msg]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
% Description: Handling all non call/cast messages.
% ----------------------------------------------------------------------------------------------------------

% Acceptor died
% -> shutdown in progress, ignore
handle_info({'EXIT', Pid, {error, {{accept_failed, {shutdown, _}}}}}, #state{acceptor = Pid} = State) -> {noreply, State};
% -> respawn listening socket and acceptor
handle_info({'EXIT', Pid, _Reason}, #state{listen_socket = ListenSocket, socket_mode = SocketMode, port = Port, acceptor = Pid, recv_timeout = RecvTimeout, custom_opts = CustomOpts} = State) ->
	?LOG_WARNING("acceptor has died with reason: ~p, respawning", [_Reason]),
	AcceptorPid = misultin_socket:start_link(ListenSocket, Port, RecvTimeout, SocketMode, CustomOpts),
	{noreply, State#state{acceptor = AcceptorPid}};

% handle_info generic fallback (ignore)
handle_info(_Info, State) ->
	?LOG_WARNING("received unknown info message: ~p", [_Info]),
	{noreply, State}.

% ----------------------------------------------------------------------------------------------------------
% Function: terminate(Reason, State) -> void()
% Description: This function is called by a gen_server when it is about to terminate. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
% ----------------------------------------------------------------------------------------------------------
terminate(_Reason, #state{listen_socket = ListenSocket, socket_mode = SocketMode, acceptor = AcceptorPid, persistent_sock_references = PersistentSockReferences}) ->
	?LOG_INFO("shutting down server with Pid ~p", [self()]),
	% kill acceptor
	exit(AcceptorPid, kill),
	% send a shutdown message to all persistent sockets, if any
	?LOG_DEBUG("sending shutdown message to persistent sockets, if any", []),
	lists:foreach(fun(PsPid) -> catch PsPid ! shutdown end, PersistentSockReferences),
	% stop tcp socket
	misultin_socket:close(ListenSocket, SocketMode),
	terminated.

% ----------------------------------------------------------------------------------------------------------
% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
% Description: Convert process state when code is changed.
% ----------------------------------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ============================ /\ GEN_SERVER CALLBACKS =====================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

% Function: -> false | IpTuple
% Description: Checks and converts a string Ip to inet repr.
check_and_convert_string_to_ip(Ip) ->
	case inet_parse:address(Ip) of
		{error, _Reason} ->
			false;
		{ok, IpTuple} ->
			IpTuple
	end.
	
% Function: -> true | false
% Description: Checks if all necessary Ssl Options have been specified
check_ssl_options(SslOptions) ->
	Opts = [verify, fail_if_no_peer_cert, verify_fun, depth, certfile, keyfile, password, cacertfile, ciphers, reuse_sessions, reuse_session],
	F = fun({Name, _Value}) ->
		?LOG_DEBUG("testing ~p", [Name]),
		case lists:member(Name, Opts) of
			false -> ?LOG_DEBUG("NOT found ~p", [Name]),false;
			_ -> ?LOG_DEBUG("found ~p", [Name]),true
		end
	end,
	lists:all(F, SslOptions).

% Description: Validate and get misultin options.
get_option({OptionName, DefaultValue, CheckAndConvertFun, FailTypeError}, Options) ->
	case proplists:get_value(OptionName, Options) of
		undefined ->
			case DefaultValue of
				{error, Reason} ->
					{error, Reason};
				Value -> 
					{OptionName, Value}
			end;
		Value ->
			case CheckAndConvertFun(Value) of
				false ->
					{error, {FailTypeError, Value}};
				true -> 
					{OptionName, Value};
				OutValue ->
					{OptionName, OutValue}
			end
	end.

% Function: -> ok | {error, Reason}
% Description: Start an application.
start_application(Application) ->
	case lists:keyfind(Application, 1, application:which_applications()) of
		false ->
			application:start(Application);
		_ ->
			ok
	end.

% Function: -> {ok, ListenSocket, AcceptorPid} | {error, Error}
% Description: Create listening socket
create_listener_and_acceptor(Port, Options, RecvTimeout, SocketMode, CustomOpts) ->
	?LOG_DEBUG("starting listening ~p socket with options ~p on port ~p", [SocketMode, Options, Port]),
	case misultin_socket:listen(Port, Options, SocketMode) of
		{ok, ListenSocket} ->
			?LOG_DEBUG("starting acceptor",[]),
			% create acceptor
			AcceptorPid = misultin_socket:start_link(ListenSocket, Port, RecvTimeout, SocketMode, CustomOpts),
			{ok, ListenSocket, AcceptorPid};
		{error, Reason} ->
			% error
			{error, Reason}
	end.

% ============================ /\ INTERNAL FUNCTIONS =======================================================
