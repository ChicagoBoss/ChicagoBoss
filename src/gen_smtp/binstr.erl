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

%% @doc Some functions for working with binary strings.

-module(binstr).

-export([
		strchr/2,
		strrchr/2,
		strpos/2,
		strrpos/2,
		substr/2,
		substr/3,
		split/3,
		split/2,
		chomp/1,
		strip/1,
		strip/2,
		strip/3,
		to_lower/1,
		to_upper/1,
		all/2,
		reverse/1,
		reverse_str_to_bin/1,
		join/2
]).

-spec strchr(Bin :: binary(), C :: char()) -> non_neg_integer().
strchr(Bin, C) when is_binary(Bin) ->
	% try to use the R14B binary module
	try binary:match(Bin, <<C>>) of
		{Index, _Length} ->
			Index + 1;
		nomatch ->
			0
	catch
		_:_ ->
			strchr(Bin, C, 0)
	end.

strchr(Bin, C, I) ->
	case Bin of
		<<_X:I/binary>> ->
			0;
		<<_X:I/binary, C, _Rest/binary>> ->
			I+1;
		_ ->
			strchr(Bin, C, I+1)
	end.


-spec strrchr(Bin :: binary(), C :: char()) -> non_neg_integer().
strrchr(Bin, C) ->
	strrchr(Bin, C, byte_size(Bin)).

strrchr(Bin, C, I) ->
	case Bin of
		<<_X:I/binary, C, _Rest/binary>> ->
			I+1;
		_ when I =< 1 ->
			0;
		_ ->
			strrchr(Bin, C, I-1)
	end.


-spec strpos(Bin :: binary(), C :: binary() | list()) -> non_neg_integer().
strpos(Bin, C) when is_binary(Bin), is_list(C) ->
	strpos(Bin, list_to_binary(C));
strpos(Bin, C) when is_binary(Bin) ->
	% try to use the R14B binary module
	try binary:match(Bin, C) of
			{Index, _Length} ->
				Index+1;
			nomatch ->
				0
	catch
		_:_ ->
			strpos(Bin, C, 0, byte_size(C))
	end.


strpos(Bin, C, I, S) ->
	case Bin of
		<<_X:I/binary>> ->
			0;
		<<_X:I/binary, C:S/binary, _Rest/binary>> ->
			I+1;
		_ ->
			strpos(Bin, C, I+1, S)
	end.


strrpos(Bin, C) ->
	strrpos(Bin, C, byte_size(Bin), byte_size(C)).

-spec strrpos(Bin :: binary(), C :: binary() | list()) -> non_neg_integer().
strrpos(Bin, C, I, S) ->
	case Bin of
		<<_X:I/binary, C:S/binary, _Rest/binary>> ->
			I+1;
		_ when I =< 1 ->
			0;
		_ ->
			strrpos(Bin, C, I-1, S)
	end.


-spec substr(Bin :: binary(), Start :: pos_integer() | neg_integer()) -> binary().
substr(<<>>, _) ->
	<<>>;
substr(Bin, Start) when Start > 0 ->
	{_, B2} = split_binary(Bin, Start-1),
	B2;
substr(Bin, Start) when Start < 0 ->
	Size = byte_size(Bin),
	{_, B2} = split_binary(Bin, Size+Start),
	B2.


-spec substr(Bin :: binary(), Start :: pos_integer() | neg_integer(), Length :: pos_integer()) -> binary().
substr(<<>>, _, _) ->
	<<>>;
substr(Bin, Start, Length) when Start > 0 ->
	{_, B2} = split_binary(Bin, Start-1),
	{B3, _} = split_binary(B2, Length),
	B3;
substr(Bin, Start, Length) when Start < 0 ->
	Size = byte_size(Bin),
	{_, B2} = split_binary(Bin, Size+Start),
	{B3, _} = split_binary(B2, Length),
	B3.


-spec split(Bin :: binary(), Separator :: binary(), SplitCount :: pos_integer()) -> [binary()].
split(Bin, Separator, SplitCount) ->
	split_(Bin, Separator, SplitCount, []).

split_(<<>>, _Separator, _SplitCount, Acc) ->
	lists:reverse(Acc);
split_(Bin, <<>>, 1, Acc) ->
	lists:reverse([Bin | Acc]);
split_(Bin, _Separator, 1, Acc) ->
	lists:reverse([Bin | Acc]);
split_(Bin, <<>>, SplitCount, Acc) ->
	split_(substr(Bin, 2), <<>>, SplitCount - 1, [substr(Bin, 1, 1) | Acc]);
split_(Bin, Separator, SplitCount, Acc) ->
	case strpos(Bin, Separator) of
		0 ->
			lists:reverse([Bin | Acc]);
		Index ->
			Head = substr(Bin, 1, Index - 1),
			Tailpresplit = substr(Bin, Index + byte_size(Separator)),
			split_(Tailpresplit, Separator, SplitCount - 1, [Head | Acc])
	end.


-spec split(Bin :: binary(), Separator :: binary()) -> [binary()].
split(Bin, Separator) ->
	% try to use the R14B binary module
	try binary:split(Bin, Separator, [global]) of
		Result ->
			case lists:last(Result) of
				<<>> ->
					lists:sublist(Result, length(Result) - 1);
				_ ->
					Result
			end
	catch
		_:_ ->
			split_(Bin, Separator, [])
	end.

split_(<<>>, _Separator, Acc) ->
	lists:reverse(Acc);
split_(Bin, <<>>, Acc) ->
	split_(substr(Bin, 2), <<>>, [substr(Bin, 1, 1) | Acc]);
split_(Bin, Separator, Acc) ->
	case strpos(Bin, Separator) of
		0 ->
			lists:reverse([Bin | Acc]);
		Index ->
			split_(substr(Bin, Index + byte_size(Separator)), Separator, [substr(Bin, 1, Index - 1) | Acc])
	end.


-spec chomp(Bin :: binary()) -> binary().
chomp(Bin) ->
	L = byte_size(Bin),
	L2 = L - 1,
	case strrpos(Bin, <<"\r\n">>) of
		L2 ->
			substr(Bin, 1,  L2 - 1);
		_ ->
			case strrchr(Bin, $\n) of
				L ->
					substr(Bin, 1, L - 1);
				_ ->
					case strrchr(Bin, $\r) of
						L ->
							substr(Bin, 1, L - 1);
						_ ->
							Bin
					end
			end
	end.


-spec strip(Bin :: binary()) -> binary().
strip(Bin) ->
	strip(Bin, both, $\s).

-spec strip(Bin :: binary(), Dir :: 'left' | 'right' | 'both') -> binary().
strip(Bin, Dir) ->
	strip(Bin, Dir, $\s).

-spec strip(Bin :: binary(), Dir :: 'left' | 'right' | 'both', C :: non_neg_integer()) -> binary().
strip(<<>>, _, _) ->
	<<>>;
strip(Bin, both, C) ->
	strip(strip(Bin, left, C), right, C);
strip(<<C, _Rest/binary>> = Bin, left, C) ->
	strip(substr(Bin, 2), left, C);
strip(Bin, left, _C) ->
	Bin;
strip(Bin, right, C) ->
	L = byte_size(Bin),
	case strrchr(Bin, C) of
		L ->
			strip(substr(Bin, 1, L - 1), right, C);
		_ ->
			Bin
	end.

-spec to_lower(Bin :: binary()) -> binary().
to_lower(Bin) ->
	to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
	Acc;
to_lower(<<H, T/binary>>, Acc) when H >= $A, H =< $Z ->
	H2 = H + 32,
	to_lower(T, <<Acc/binary, H2>>);
to_lower(<<H, T/binary>>, Acc) ->
	to_lower(T, <<Acc/binary, H>>).


-spec to_upper(Bin :: binary()) -> binary().
to_upper(Bin) ->
	to_upper(Bin, <<>>).

to_upper(<<>>, Acc) ->
	Acc;
to_upper(<<H, T/binary>>, Acc) when H >= $a, H =< $z ->
	H2 = H - 32,
	to_upper(T, <<Acc/binary, H2>>);
to_upper(<<H, T/binary>>, Acc) ->
	to_upper(T, <<Acc/binary, H>>).

-spec all(Fun :: function(), Binary :: binary()) -> boolean().
all(_Fun, <<>>) ->
	true;
all(Fun, Binary) ->
	Res = << <<X/integer>> || <<X>> <= Binary, Fun(X) >>,
	Binary == Res.
%all(Fun, <<H, Tail/binary>>) ->
%	Fun(H) =:= true andalso all(Fun, Tail).

%% this is a cool hack to very quickly reverse a binary
-spec reverse(Bin :: binary()) -> binary().
reverse(Bin) ->
	Size = byte_size(Bin)*8,
	<<T:Size/integer-little>> = Bin,
	<<T:Size/integer-big>>.

%% reverse a string into a binary - can be faster than lists:reverse on large
%% lists, even if you run binary_to_string on the result. For smaller strings
%% it's probably slower (but still not that bad).
-spec reverse_str_to_bin(String :: string()) -> binary().
reverse_str_to_bin(String) ->
	reverse(list_to_binary(String)).

-spec join(Binaries :: [binary()|list()], Glue :: binary() | list()) -> binary().
join(Binaries, Glue) ->
	join(Binaries, Glue, []).

join([H], _Glue, Acc) ->
	list_to_binary(lists:reverse([H | Acc]));
join([H|T], Glue, Acc) ->
	join(T, Glue, [Glue, H | Acc]).
