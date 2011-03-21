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

%% @doc Module with some general utility functions for SMTP.

-module(smtp_util).
-compile(export_all).

-include_lib("kernel/src/inet_dns.hrl").

%% @doc returns a sorted list of mx servers for `Domain', lowest distance first
mxlookup(Domain) ->
	case whereis(inet_db) of
		P when is_pid(P) ->
			ok;
		_ -> 
			inet_db:start()
	end,
	case lists:keyfind(nameserver, 1, inet_db:get_rc()) of
		false ->
			% we got no nameservers configured, suck in resolv.conf
			inet_config:do_load_resolv(os:type(), longnames);
		_ ->
			ok
	end,
	case inet_res:lookup(Domain, in, ?S_MX) of
		[] ->
			[];
		Result ->
			lists:sort(fun({Pref, _Name}, {Pref2, _Name2}) -> Pref =< Pref2 end, Result)
	end.

%% @doc guess the current host's fully qualified domain name
guess_FQDN() ->
	{ok, Hostname} = inet:gethostname(),
	{ok, Hostent} = inet:gethostbyname(Hostname),
	{hostent, FQDN, _Aliases, inet, _, _Addresses} = Hostent,
	FQDN.

%% @doc Compute the CRAM digest of `Key' and `Data'
-spec(compute_cram_digest/2 :: (Key :: binary(), Data :: string()) -> binary()).
compute_cram_digest(Key, Data) ->
	Bin = crypto:md5_mac(Key, Data),
	list_to_binary([io_lib:format("~2.16.0b", [X]) || <<X>> <= Bin]).

-spec(get_cram_string/1 :: (Hostname :: string()) -> string()).
get_cram_string(Hostname) ->
	binary_to_list(base64:encode(lists:flatten(io_lib:format("<~B.~B@~s>", [crypto:rand_uniform(0, 4294967295), crypto:rand_uniform(0, 4294967295), Hostname])))).

%% @doc Trim \r\n from `String'
-spec(trim_crlf/1 :: (String :: string()) -> string()).
trim_crlf(String) ->
	string:strip(string:strip(String, right, $\n), right, $\r).

-define(DAYS, ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]).
-define(MONTHS, ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]).

rfc5322_timestamp() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
	NDay = calendar:day_of_the_week(Year, Month, Day),
	DoW = lists:nth(NDay, ?DAYS),
	MoY = lists:nth(Month, ?MONTHS),
	io_lib:format("~s, ~b ~s ~b ~b:~b:~b ~s", [DoW, Day, MoY, Year, Hour, Minute, Second, zone()]).

% borrowed from YAWS
zone() ->
	Time = erlang:universaltime(),
	LocalTime = calendar:universal_time_to_local_time(Time),
	DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
	calendar:datetime_to_gregorian_seconds(Time),
	zone((DiffSecs/3600)*100).

%% Ugly reformatting code to get times like +0000 and -1300

zone(Val) when Val < 0 ->
	io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
	io_lib:format("+~4..0w", [trunc(abs(Val))]).

generate_message_id() ->
	FQDN = guess_FQDN(),
	Md5 = [io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary([erlang:now(), FQDN]))],
	io_lib:format("<~s@~s>", [Md5, FQDN]).

generate_message_boundary() ->
	FQDN = guess_FQDN(),
	["_=", [io_lib:format("~2.36.0b", [X]) || <<X>> <= erlang:md5(term_to_binary([erlang:now(), FQDN]))], "=_"].



