%%%-------------------------------------------------------------------
%%% File    : principe.hrl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 Sept 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------


%% Standard definitions
-define(TSERVER, "localhost").
-define(TPORT, 1978).
-define(TOPTS, [binary, {packet, 0}, {nodelay, true}, {active, true}, {keepalive, true}]).
-define(TIMEOUT, 5000).


-define(NULL, <<0:8>>).

%% Tyrant protocol constants
-define(PUT, 16#C810).
-define(PUTKEEP, 16#C811).
-define(PUTCAT, 16#C812).
-define(PUTSHL, 16#C813).
-define(PUTNR, 16#C818).
-define(OUT, 16#C820).
-define(GET, 16#C830).
-define(MGET, 16#C831).
-define(VSIZ, 16#C838).
-define(ITERINIT, 16#C850).
-define(ITERNEXT, 16#C851).
-define(FWMKEYS, 16#C858).
-define(ADDINT, 16#C860).
-define(ADDDOUBLE, 16#C861).
-define(EXT, 16#C868).
-define(SYNC, 16#C870).
-define(OPTIMIZE, 16#C871).
-define(VANISH, 16#C872).
-define(COPY, 16#C873).
-define(RESTORE, 16#C874).
-define(SETMST, 16#C878).
-define(RNUM, 16#C880).
-define(SIZE, 16#C881).
-define(STAT, 16#C888).
-define(MISC, 16#C890).
-define(REPL, 16#C8A0).

-define(MONOULOG, 1 bsl 0).
-define(XOLCKREC, 1 bsl 0).
-define(XOLCKGLB, 1 bsl 1).

%% Constants for tyrant tables
-define(ITLEXICAL, "0").
-define(ITDECIMAL, "1").
-define(ITOPT, "9998").
-define(ITVOID, "9999").

-define(QCSTREQ, 0).
-define(QCSTRINC, 1).
-define(QCSTRBW, 2).
-define(QCSTREW, 3).
-define(QCSTRAND, 4).
-define(QCSTROR, 5).
-define(QCSTROREQ, 6).
-define(QCSTRRX, 7).
-define(QCNUMEQ, 8).
-define(QCNUMGT, 9).
-define(QCNUMGE, 10).
-define(QCNUMLT, 11).
-define(QCNUMLE, 12).
-define(QCNUMBT, 13).
-define(QCNUMOREQ, 14).
-define(QCNEGATE, 1 bsl 24).
-define(QCNOIDX, 1 bsl 25).

-define(QOSTRASC, 0).
-define(QOSTRDESC, 1).
-define(QONUMASC, 2).
-define(QONUMDESC, 3).

%% Macros for function patterns that are used frequently.
-define(T0(Code), gen_tcp:send(Socket, [<<Code:16>>])).
-define(T1(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, Key])).
-define(T2(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>, Key, Value])).

-define(TSimple(Func, Args), case principe:misc(Socket, Func, Args) of 
				 [] -> ok; 
				 Error -> Error 
			     end).
-define(TRaw(Func, Args), principe:misc(Socket, Func, Args)).

-define(R_SUCCESS, tyrant_response(fun recv_success/1)).
-define(R_INT32, tyrant_response(fun recv_size/1)).
-define(R_INT64, tyrant_response(fun recv_size64/1)).
-define(R_SIZE_DATA, tyrant_response(fun recv_size_data/1)).
-define(R_SIZE64_SIZE64, tyrant_response(fun recv_size64_size64/1)).
-define(R_2TUPLE, tyrant_response(fun recv_count_2tuple/1)).
-define(R_4TUPLE, tyrant_response(fun recv_count_4tuple/1)).



%% For testing
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
