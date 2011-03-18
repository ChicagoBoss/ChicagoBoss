%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.

-module(pgsql_fdatetime).

-export([decode/2, encode/2]).

-define(int32, 1/big-signed-unit:32).

-define(postgres_epoc_jdate, 2451545).

-define(mins_per_hour, 60).
-define(secs_per_day, 86400.0).
-define(secs_per_hour, 3600.0).
-define(secs_per_minute, 60.0).

decode(date, <<J:1/big-signed-unit:32>>)             -> j2date(?postgres_epoc_jdate + J);
decode(time, <<N:1/big-float-unit:64>>)              -> f2time(N);
decode(timetz, <<N:1/big-float-unit:64, TZ:?int32>>) -> {f2time(N), TZ};
decode(timestamp, <<N:1/big-float-unit:64>>)         -> f2timestamp(N);
decode(timestamptz, <<N:1/big-float-unit:64>>)       -> f2timestamp(N);
decode(interval, <<N:1/big-float-unit:64, D:?int32, M:?int32>>) -> {f2time(N), D, M}.

encode(date, D)         -> <<4:?int32, (date2j(D) - ?postgres_epoc_jdate):1/big-signed-unit:32>>;
encode(time, T)         -> <<8:?int32, (time2f(T)):1/big-float-unit:64>>;
encode(timetz, {T, TZ}) -> <<12:?int32, (time2f(T)):1/big-float-unit:64, TZ:?int32>>;
encode(timestamp, TS)   -> <<8:?int32, (timestamp2f(TS)):1/big-float-unit:64>>;
encode(timestamptz, TS) -> <<8:?int32, (timestamp2f(TS)):1/big-float-unit:64>>;
encode(interval, {T, D, M}) -> <<16:?int32, (time2f(T)):1/big-float-unit:64, D:?int32, M:?int32>>.

j2date(N) ->
    J = N + 32044,
    Q1 = J div 146097,
    Extra = (J - Q1 * 146097) * 4 + 3,
    J2 = J + 60 + Q1 * 3 + Extra div 146097,
    Q2 = J2 div 1461,
    J3 = J2 - Q2 * 1461,
    Y = J3 * 4 div 1461,
    case Y of
        0 -> J4 = ((J3 + 306) rem 366) + 123;
        _ -> J4 = ((J3 + 305) rem 365) + 123
    end,
    Year = (Y + Q2 * 4) - 4800,
    Q3 = J4 * 2141 div 65536,
    Day = J4 - 7834 * Q3 div 256,
    Month = (Q3 + 10) rem 12 + 1,
    {Year, Month, Day}.

date2j({Y, M, D}) ->
    case M > 2 of
        true ->
            M2 = M + 1,
            Y2 = Y + 4800;
        false ->
            M2 = M + 13,
            Y2 = Y + 4799
    end,
    C = Y2 div 100,
    J1 = Y2 * 365 - 32167,
    J2 = J1 + (Y2 div 4 - C + C div 4),
    J2 + 7834 * M2 div 256 + D.

f2time(N) ->
    {R1, Hour} = tmodulo(N, ?secs_per_hour),
    {R2, Min}  = tmodulo(R1, ?secs_per_minute),
    {R3, Sec}  = tmodulo(R2, 1.0),
    case timeround(R3) of
        US when US >= 1.0 -> f2time(ceiling(N));
        US                -> {Hour, Min, Sec + US}
    end.

time2f({H, M, S}) ->
    ((H * ?mins_per_hour + M) * ?secs_per_minute) + S.

f2timestamp(N) ->
    case tmodulo(N, ?secs_per_day) of
        {T, D} when T < 0 -> f2timestamp2(D - 1 + ?postgres_epoc_jdate, T + ?secs_per_day);
        {T, D}            -> f2timestamp2(D + ?postgres_epoc_jdate, T)
    end.

f2timestamp2(D, T) ->
    {_H, _M, S} = Time = f2time(T),
    Date = j2date(D),
    case tsround(S - trunc(S)) of
        N when N >= 1.0 ->
            case ceiling(T) of
                T2 when T2 > ?secs_per_day -> f2timestamp2(D + 1, 0.0);
                T2                         -> f2timestamp2(T2, D)
            end;
        _ -> ok
    end,
    {Date, Time}.

timestamp2f({Date, Time}) ->
    D = date2j(Date) - ?postgres_epoc_jdate,
    D * ?secs_per_day + time2f(Time).

tmodulo(T, U) ->
    case T < 0 of
        true  -> Q = ceiling(T / U);
        false -> Q = floor(T / U)
    end,
    case Q of
        0 -> {T, Q};
        _ -> {T - rint(Q * U), Q}
    end.

rint(N)      -> round(N) * 1.0.
timeround(J) -> rint(J * 10000000000.0) / 10000000000.0.
tsround(J)   -> rint(J * 1000000.0) / 1000000.0.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        N when N < 0 -> T - 1;
        N when N > 0 -> T;
        _            -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        N when N < 0 -> T;
        N when N > 0 -> T + 1;
        _            -> T
    end.
