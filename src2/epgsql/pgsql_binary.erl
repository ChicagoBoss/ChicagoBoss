%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.

-module(pgsql_binary).

-export([encode/2, decode/2, supports/1]).

-define(int32, 1/big-signed-unit:32).
-define(datetime, (get(datetime_mod))).

encode(_Any, null)  -> <<-1:?int32>>;
encode(bool, true)  -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, false) -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(int2, N)     -> <<2:?int32, N:1/big-signed-unit:16>>;
encode(int4, N)     -> <<4:?int32, N:1/big-signed-unit:32>>;
encode(int8, N)     -> <<8:?int32, N:1/big-signed-unit:64>>;
encode(float4, N)   -> <<4:?int32, N:1/big-float-unit:32>>;
encode(float8, N)   -> <<8:?int32, N:1/big-float-unit:64>>;
encode(bpchar, C) when is_integer(C) -> <<1:?int32, C:1/big-unsigned-unit:8>>;
encode(bpchar, B) when is_binary(B)  -> <<(byte_size(B)):?int32, B/binary>>;
encode(Type, B) when Type == time; Type == timetz          -> ?datetime:encode(Type, B);
encode(Type, B) when Type == date; Type == timestamp       -> ?datetime:encode(Type, B);
encode(Type, B) when Type == timestamptz; Type == interval -> ?datetime:encode(Type, B);
encode(bytea, B) when is_binary(B)   -> <<(byte_size(B)):?int32, B/binary>>;
encode(text, B) when is_binary(B)    -> <<(byte_size(B)):?int32, B/binary>>;
encode(varchar, B) when is_binary(B) -> <<(byte_size(B)):?int32, B/binary>>;
encode(Type, L) when is_list(L)      -> encode(Type, list_to_binary(L));
encode(_Type, _Value)                -> {error, unsupported}.

decode(bool, <<1:1/big-signed-unit:8>>)     -> true;
decode(bool, <<0:1/big-signed-unit:8>>)     -> false;
decode(bpchar, <<C:1/big-unsigned-unit:8>>) -> C;
decode(int2, <<N:1/big-signed-unit:16>>)    -> N;
decode(int4, <<N:1/big-signed-unit:32>>)    -> N;
decode(int8, <<N:1/big-signed-unit:64>>)    -> N;
decode(float4, <<N:1/big-float-unit:32>>)   -> N;
decode(float8, <<N:1/big-float-unit:64>>)   -> N;
decode(record, <<_:?int32, Rest/binary>>)   -> list_to_tuple(decode_record(Rest, []));
decode(Type, B) when Type == time; Type == timetz          -> ?datetime:decode(Type, B);
decode(Type, B) when Type == date; Type == timestamp       -> ?datetime:decode(Type, B);
decode(Type, B) when Type == timestamptz; Type == interval -> ?datetime:decode(Type, B);
decode(_Other, Bin) -> Bin.

decode_record(<<>>, Acc) ->
    lists:reverse(Acc);
decode_record(<<_Type:?int32, -1:?int32, Rest/binary>>, Acc) ->
    decode_record(Rest, [null | Acc]);
decode_record(<<Type:?int32, Len:?int32, Value:Len/binary, Rest/binary>>, Acc) ->
    Value2 = decode(pgsql_types:oid2type(Type), Value),
    decode_record(Rest, [Value2 | Acc]).

supports(bool)    -> true;
supports(bpchar)  -> true;
supports(int2)    -> true;
supports(int4)    -> true;
supports(int8)    -> true;
supports(float4)  -> true;
supports(float8)  -> true;
supports(bytea)   -> true;
supports(text)    -> true;
supports(varchar) -> true;
supports(record)  -> true;
supports(date)    -> true;
supports(time)    -> true;
supports(timetz)  -> true;
supports(timestamp)   -> true;
supports(timestamptz) -> true;
supports(interval)    -> true;
supports(_Type)       -> false.
