%% Copyright (c) 2009 
%% Nick Gerakines <nick@gerakines.net>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc A protcol buffers encoding and decoding module.
-module(protobuffs).

%% Pubic
-export([encode/3, encode_packed/3, decode/2, decode_packed/2]).

%% Used by generated *_pb file. Not intended to used by User
-export([next_field_num/1]).

%% Will be removed from export, only intended for internal usage
-deprecated([{read_field_num_and_wire_type,1,next_version}]).
-deprecated([{decode_value,3,next_version}]).
-export([read_field_num_and_wire_type/1, decode_value/3]).

-define(TYPE_VARINT, 0).
-define(TYPE_64BIT, 1).
-define(TYPE_STRING, 2).
-define(TYPE_START_GROUP, 3).
-define(TYPE_END_GROUP, 4).
-define(TYPE_32BIT, 5).

-type encoded_field_type() ::
	?TYPE_VARINT | ?TYPE_64BIT | ?TYPE_STRING |
	?TYPE_START_GROUP | ?TYPE_END_GROUP | ?TYPE_32BIT.

-type field_type() :: bool | enum | int32 | uint32 | int64 | 
		      unit64 | sint32 | sint64 | fixed32 | 
		      sfixed32 | fixed64 | sfixed64 | string | 
		      bytes | float | double.

%%--------------------------------------------------------------------
%% @doc Encode an Erlang data structure into a Protocol Buffers value.
%% @end
%%--------------------------------------------------------------------
-spec encode(FieldID :: non_neg_integer(),
	     Value :: any(),
	     Type :: field_type()) -> 
		    binary().

encode(FieldID, Value, Type) ->
    iolist_to_binary(encode_internal(FieldID, Value, Type)).

%%--------------------------------------------------------------------
%% @doc Encode an list of Erlang data structure into a Protocol Buffers values.
%% @end
%%--------------------------------------------------------------------
-spec encode_packed(FieldID :: non_neg_integer(), 
		    Values :: list(), 
		    Type :: field_type()) -> 
			   binary().
encode_packed(_FieldID, [], _Type) ->
    <<>>;
encode_packed(FieldID, Values, Type) ->
    PackedValues = iolist_to_binary(encode_packed_internal(Values,Type,[])),
    Size = encode_varint(size(PackedValues)),
    iolist_to_binary([encode_field_tag(FieldID, ?TYPE_STRING),Size,PackedValues]).
    
%% @hidden
-spec encode_internal(FieldID :: non_neg_integer(), 
		      Value :: any(), 
		      Type :: field_type()) -> 
			     iolist().
encode_internal(FieldID, false, bool) ->
    encode_internal(FieldID, 0, int32);
encode_internal(FieldID, true, bool) ->
    encode_internal(FieldID, 1, int32);
encode_internal(FieldID, Integer, enum) ->
    encode_internal(FieldID, Integer, uint32);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer < 0 ->
    encode_internal(FieldID, Integer, int64);
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_internal(FieldID, Integer + (1 bsl 64), uint64);
encode_internal(FieldID, Integer, int32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint32) when Integer band 16#ffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);    
encode_internal(FieldID, Integer, int64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, uint64) when Integer band 16#ffffffffffffffff =:= Integer ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, bool) when Integer band 1 =:= 1 ->
    encode_varint_field(FieldID, Integer);
encode_internal(FieldID, Integer, sint32) when Integer >= -16#80000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint64) when Integer >= -16#8000000000000000, Integer < 0 ->
    encode_varint_field(FieldID, bnot (Integer bsl 1));
encode_internal(FieldID, Integer, sint32) when Integer >= 0, Integer =< 16#7fffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, sint64) when Integer >= 0, Integer =< 16#7fffffffffffffff ->
    encode_varint_field(FieldID, Integer bsl 1);
encode_internal(FieldID, Integer, fixed32) when Integer band 16#ffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, sfixed32) when Integer >= -16#80000000, Integer =< 16#7fffffff ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Integer:32/little-integer>>];
encode_internal(FieldID, Integer, fixed64) when Integer band 16#ffffffffffffffff =:= Integer ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, Integer, sfixed64) when Integer >= -16#8000000000000000, Integer =< 16#7fffffffffffffff ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Integer:64/little-integer>>];
encode_internal(FieldID, String, string) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), string);
encode_internal(FieldID, String, string) when is_binary(String) ->
    encode_internal(FieldID, String, bytes);
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), bytes);
encode_internal(FieldID, Bytes, bytes) when is_binary(Bytes) ->
    [encode_field_tag(FieldID, ?TYPE_STRING), encode_varint(size(Bytes)), Bytes];
encode_internal(FieldID, String, bytes) when is_list(String) ->
    encode_internal(FieldID, list_to_binary(String), bytes);
encode_internal(FieldID, Float, float) when is_integer(Float) ->
    encode_internal(FieldID, Float + 0.0, float);
encode_internal(FieldID, Float, float) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<Float:32/little-float>>];
encode_internal(FieldID, nan, float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<0:16,192:8,255:8>>];
encode_internal(FieldID, infinity, float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<0:16,128:8,127:8>>];
encode_internal(FieldID, '-infinity', float) ->
    [encode_field_tag(FieldID, ?TYPE_32BIT), <<0:16,128:8,255:8>>];
encode_internal(FieldID, Float, double) when is_float(Float) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<Float:64/little-float>>];
encode_internal(FieldID, nan, double) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<0:48,16#F8,16#FF>>];
encode_internal(FieldID, infinity, double) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<0:48,16#F0,16#7F>>];
encode_internal(FieldID, '-infinity', double) ->
    [encode_field_tag(FieldID, ?TYPE_64BIT), <<0:48,16#F0,16#FF>>].


%% @hidden
-spec encode_packed_internal(Values :: list(), 
			     ExpectedType :: field_type(),
			     Acc :: list()) ->
				    iolist().
encode_packed_internal([],_Type,Acc) ->
    lists:reverse(Acc);
encode_packed_internal([Value|Tail], ExpectedType, Acc) ->
    [_|V] = encode_internal(1, Value, ExpectedType),
    encode_packed_internal(Tail, ExpectedType, [V|Acc]).

%%--------------------------------------------------------------------
%% @doc Will be hidden in future releases
%% @end
%%--------------------------------------------------------------------
-spec read_field_num_and_wire_type(Bytes :: binary()) ->
					  {{non_neg_integer(), encoded_field_type()}, binary()}.
read_field_num_and_wire_type(Bytes) ->
    {Tag, Rest} = decode_varint(Bytes),
    FieldID = Tag bsr 3,
    WireType = Tag band 7,
    {{FieldID, WireType}, Rest}.
    
%%--------------------------------------------------------------------
%% @doc Decode a singel value from a protobuffs data structure
%% @end
%%--------------------------------------------------------------------
-spec decode(Bytes :: binary(), ExpectedType :: field_type()) ->
		    {{non_neg_integer(), any()}, binary()}.
decode(Bytes, ExpectedType) ->
    {{FieldID, WireType}, Rest} = read_field_num_and_wire_type(Bytes),
    {Value, Rest1} = decode_value(Rest, WireType, ExpectedType),
    {{FieldID, Value}, Rest1}.

%%--------------------------------------------------------------------
%% @doc Decode packed values from a protobuffs data structure
%% @end
%%--------------------------------------------------------------------
-spec decode_packed(Bytes :: binary(), ExpectedType :: field_type()) ->
			   {{non_neg_integer(), any()}, binary()}.
decode_packed(Bytes, ExpectedType) ->
    {{FieldID, ?TYPE_STRING}, Rest} = read_field_num_and_wire_type(Bytes),
    {Length, Rest1} = decode_varint(Rest),
    {Packed,Rest2} = split_binary(Rest1, Length),
    Values = decode_packed_values(Packed, ExpectedType, []),
    {{FieldID, Values},Rest2}.

%%--------------------------------------------------------------------
%% @doc Returns the next field number id from a protobuffs data structure
%% @end
%%--------------------------------------------------------------------
-spec next_field_num(Bytes :: binary()) ->
			    {ok,non_neg_integer()}.
next_field_num(Bytes) ->
    {{FieldID,_WiredType}, _Rest} = read_field_num_and_wire_type(Bytes),
    {ok,FieldID}.
    
%% @hidden    
-spec decode_packed_values(Bytes :: binary(),
			   Type :: field_type(),
			   Acc :: list()) ->
				  iolist().
decode_packed_values(<<>>, _, Acc) ->
    lists:reverse(Acc);
decode_packed_values(Bytes, bool, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, bool),
    decode_packed_values(Rest, bool, [Value|Acc]);
decode_packed_values(Bytes, enum, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, enum),
    decode_packed_values(Rest, bool, [Value|Acc]);
decode_packed_values(Bytes, int32, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, int32),
    decode_packed_values(Rest, int32, [Value|Acc]);
decode_packed_values(Bytes, uint32, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, uint32),
    decode_packed_values(Rest, uint32, [Value|Acc]);
decode_packed_values(Bytes, sint32, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, sint32),
    decode_packed_values(Rest, sint32, [Value|Acc]);
decode_packed_values(Bytes, int64, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, int64),
    decode_packed_values(Rest, int64, [Value|Acc]);
decode_packed_values(Bytes, uint64, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, uint64),
    decode_packed_values(Rest, uint64, [Value|Acc]);
decode_packed_values(Bytes, sint64, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_VARINT, sint64),
    decode_packed_values(Rest, sint64, [Value|Acc]);
decode_packed_values(Bytes, float, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_32BIT, float),
    decode_packed_values(Rest, float, [Value|Acc]);
decode_packed_values(Bytes, double, Acc) ->
    {Value,Rest} = decode_value(Bytes,?TYPE_64BIT, double),
    decode_packed_values(Rest, double, [Value|Acc]).


%%--------------------------------------------------------------------
%% @doc Will be hidden in future releases
%% @end
%%--------------------------------------------------------------------
-spec decode_value(Bytes :: binary(),
		   WireType :: encoded_field_type(),
		   ExpectedType :: field_type()) ->
			  {any(),binary()}.
decode_value(Bytes, ?TYPE_VARINT, ExpectedType) ->
    {Value, Rest} = decode_varint(Bytes),
    {typecast(Value, ExpectedType), Rest};
decode_value(Bytes, ?TYPE_STRING, string) ->
    {Length, Rest} = decode_varint(Bytes),
    {Value,Rest1} = split_binary(Rest, Length),
    {binary_to_list(Value),Rest1};
decode_value(Bytes, ?TYPE_STRING, bytes) ->
    {Length, Rest} = decode_varint(Bytes),
    split_binary(Rest, Length);
decode_value(<<Value:64/little-unsigned-integer, Rest/binary>>, ?TYPE_64BIT, fixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, _:32, Rest/binary>>, ?TYPE_64BIT, fixed32) ->
    {Value, Rest};
decode_value(<<Value:64/little-signed-integer, Rest/binary>>, ?TYPE_64BIT, sfixed64) ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, _:32, Rest/binary>>, ?TYPE_64BIT, sfixed32) ->
    {Value, Rest};
decode_value(<<Value:32/little-unsigned-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= fixed32; Type =:= fixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-signed-integer, Rest/binary>>, ?TYPE_32BIT, Type) when Type =:= sfixed32; Type =:= sfixed64 ->
    {Value, Rest};
decode_value(<<Value:32/little-float, Rest/binary>>, ?TYPE_32BIT, float) ->
    {Value + 0.0, Rest};
decode_value(<<0:16, 128:8, 127:8, Rest/binary>>, ?TYPE_32BIT, float) ->
    {infinity, Rest};
decode_value(<<0:16, 128:8, 255:8, Rest/binary>>, ?TYPE_32BIT, float) ->
    {'-infinity', Rest};
decode_value(<<_:16, 2#1:1, _:7, _:1, 2#1111111:7, Rest/binary>>, ?TYPE_32BIT, float) ->
    {nan, Rest};
decode_value(<<Value:64/little-float, Rest/binary>>, ?TYPE_64BIT, double) ->
    {Value + 0.0, Rest};
decode_value(<<0:48, 240:8, 127:8, Rest/binary>>, ?TYPE_64BIT, double) ->
    {infinity, Rest};
decode_value(<<0:48, 240:8, 255:8, Rest/binary>>, ?TYPE_64BIT, double) ->
    {'-infinity', Rest};
decode_value(<<_:48, 2#1111:4, _:4, _:1, 2#1111111:7, Rest/binary>>, ?TYPE_64BIT, double) ->
    {nan, Rest};
decode_value(Value, WireType, ExpectedType) ->
    exit({error, {unexpected_value, WireType, ExpectedType, Value}}).

%% @hidden
-spec typecast(Value :: any(), Type :: field_type()) ->
		      any().
typecast(Value, SignedType) when SignedType =:= int32; SignedType =:= int64 ->
    if
        Value band 16#8000000000000000 =/= 0 -> Value - 16#10000000000000000;
        true -> Value
    end;
typecast(Value, SignedType) when SignedType =:= sint32; SignedType =:= sint64 ->
    (Value bsr 1) bxor (-(Value band 1));
typecast(Value, Type) when Type =:= bool -> 
    Value =:= 1;
typecast(Value, _) ->
    Value.

%% @hidden
-spec encode_field_tag(FieldID :: non_neg_integer(), 
		       FieldType :: encoded_field_type()) ->
			      binary().
encode_field_tag(FieldID, FieldType) when FieldID band 16#3fffffff =:= FieldID ->
    encode_varint((FieldID bsl 3) bor FieldType).

%% @hidden
-spec encode_varint_field(FieldID :: non_neg_integer(),
			  Integer :: integer()) ->
				 iolist().
encode_varint_field(FieldID, Integer) ->
    [encode_field_tag(FieldID, ?TYPE_VARINT), encode_varint(Integer)].

%% @hidden
-spec encode_varint(I :: integer()) ->
			   binary().
encode_varint(I) ->
    encode_varint(I, []).

%% @hidden
-spec encode_varint(I :: integer(), Acc :: list()) ->
			   binary().
encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).

%% @hidden
-spec decode_varint(Bytes :: binary()) ->
			   {integer(), binary()}.
decode_varint(Bytes) ->
    decode_varint(Bytes, []).

-spec decode_varint(Bytes :: binary(), list()) -> 
			   {integer(), binary()}.
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {Result, Rest};
decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]).
