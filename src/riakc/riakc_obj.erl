%% -------------------------------------------------------------------
%%
%% riakc_obj: Container for Riak data and metadata
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc riakc_obj is used to wrap bucket/key/value data sent to the
%%                server on put and received on get.  It provides
%%                accessors for retrieving the data and metadata
%%                and copes with siblings if multiple values are allowed.


-module(riakc_obj).
-export([new/2, new/3, new/4,
         bucket/1,
         key/1,
         vclock/1,
         value_count/1,
         select_sibling/2,
         get_contents/1,
         get_metadata/1,
         get_metadatas/1,
         get_content_type/1,
         get_content_types/1,
         get_value/1,
         get_values/1,
         update_metadata/2,
         update_value/2,
         update_value/3,
         update_content_type/2,
         get_update_metadata/1,
         get_update_content_type/1,
         get_update_value/1,
         md_ctype/1,
         set_vclock/2
        ]).
%% Internal library use only
-export([new_obj/4]).

-include("riakc_obj.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type bucket() :: binary().
-type key() :: binary() | 'undefined'.
-type vclock() :: binary().
-type metadata() :: dict().
-type content_type() :: string().
-type value() :: binary().
-type contents() :: [{metadata(), value()}].

-record(riakc_obj, {
          bucket :: bucket(),
          key :: key(),
          vclock :: vclock(),
          contents :: contents(),
          updatemetadata :: dict(),
          updatevalue :: value()
         }).

%% ====================================================================
%% object functions
%% ====================================================================

%% @doc Constructor for new riak client objects.
-spec new(bucket(), key()) -> #riakc_obj{}.
new(Bucket, Key) ->
    #riakc_obj{bucket = Bucket, key = Key, contents = []}.

%% @doc Constructor for new riak client objects with an update value
-spec new(bucket(), key(), value()) -> #riakc_obj{}.
new(Bucket, Key, Value) ->
    #riakc_obj{bucket = Bucket, key = Key, contents = [], updatevalue = Value}.

%% @doc Constructor for new riak client objects with an update value
-spec new(bucket(), key(), value(), content_type()) -> #riakc_obj{}.
new(Bucket, Key, Value, ContentType) ->
    O = #riakc_obj{bucket = Bucket, key = Key, contents = [], updatevalue = Value},
    update_content_type(O, ContentType).


%% @doc Return the containing bucket for this riakc_obj.
-spec bucket(#riakc_obj{}) -> bucket().
bucket(O) ->
    O#riakc_obj.bucket.

%% @spec key(riakc_obj()) -> key()
%% @doc  Return the key for this riakc_obj.
-spec key(#riakc_obj{}) -> key().
key(O) ->
    O#riakc_obj.key.

%% @doc  Return the vector clock for this riakc_obj.
-spec vclock(#riakc_obj{}) -> vclock().
vclock(O) ->
    O#riakc_obj.vclock.

%% @doc  Return the number of values (siblings) of this riakc_obj.
-spec value_count(#riakc_obj{}) -> non_neg_integer().
value_count(#riakc_obj{contents=Contents}) -> 
    length(Contents).

%% @doc  Select the sibling to use for update - starting from 1.
-spec select_sibling(pos_integer(), #riakc_obj{}) -> #riakc_obj{}.
select_sibling(Index, O) ->
    {MD,V} = lists:nth(Index, O#riakc_obj.contents),
    O#riakc_obj{updatemetadata=MD, updatevalue=V}.

%% @doc  Return the contents (a list of {metadata, value} tuples) for
%%       this riakc_obj.
-spec get_contents(#riakc_obj{}) -> contents().
get_contents(O) ->
    O#riakc_obj.contents.

%% @doc  Assert that this riak_object has no siblings and return its associated
%%       metadata.  This function will throw siblings if the object has 
%%       siblings (value_count() > 1).
-spec get_metadata(#riakc_obj{}) -> metadata().
get_metadata(O=#riakc_obj{}) ->
    case get_contents(O) of
        [] ->
            throw(no_metadata);
        [{MD,_V}] ->
            MD;
        _ ->
            throw(siblings)
    end.

%% @doc  Return a list of the metadata values for this riak_object.
-spec get_metadatas(#riakc_obj{}) -> [metadata()].
get_metadatas(#riakc_obj{contents=Contents}) ->
    [M || {M,_V} <- Contents].

%% @doc Return the content type of the value if there are no siblings
-spec get_content_type(#riakc_obj{}) -> content_type().
get_content_type(Object=#riakc_obj{}) ->
    UM = get_metadata(Object),
    md_ctype(UM).

%% @doc Return a list of content types for all siblings
-spec get_content_types(#riakc_obj{}) -> [content_type()].
get_content_types(Object=#riakc_obj{}) ->
    F = fun({M,_}) -> md_ctype(M) end,
    [F(C) || C<- get_contents(Object)].


%% @doc  Assert that this riakc_obj has no siblings and return its associated
%%       value.  This function will throw siblings if the object has 
%%       siblings (value_count() > 1).
-spec get_value(#riakc_obj{}) -> value().
get_value(#riakc_obj{}=O) ->
    case get_contents(O) of
        [] ->
            throw(no_value);
        [{_MD,V}] ->
            V;
        _ ->
            throw(siblings)
    end.

%% @doc  Return a list of object values for this riakc_obj.
-spec get_values(#riakc_obj{}) -> [value()].
get_values(#riakc_obj{contents=Contents}) ->
    [V || {_,V} <- Contents].

%% @doc  Set the updated metadata of an object to M.
-spec update_metadata(#riakc_obj{}, metadata()) -> #riakc_obj{}.
update_metadata(Object=#riakc_obj{}, M) ->
    Object#riakc_obj{updatemetadata=M}.

%% @doc  Set the updated content-type of an object to CT.
-spec update_content_type(#riakc_obj{},content_type()|binary()) -> #riakc_obj{}.
update_content_type(Object=#riakc_obj{}, CT) when is_binary(CT) ->
    update_content_type(Object, binary_to_list(CT));
update_content_type(Object=#riakc_obj{}, CT) when is_list(CT) ->
    M1 = get_update_metadata(Object),
    Object#riakc_obj{updatemetadata=dict:store(?MD_CTYPE, CT, M1)}.

%% @doc  Set the updated value of an object to V
-spec update_value(#riakc_obj{}, value()) -> #riakc_obj{}.
update_value(Object=#riakc_obj{}, V) -> Object#riakc_obj{updatevalue=V}.

%% @doc  Set the updated value of an object to V
-spec update_value(#riakc_obj{}, value(), content_type()) -> #riakc_obj{}.
update_value(Object=#riakc_obj{}, V, CT) -> 
    O1 = update_content_type(Object, CT),
    O1#riakc_obj{updatevalue=V}.

%% @doc  Return the updated metadata of this riakc_obj.
-spec get_update_metadata(#riakc_obj{}) -> metadata().
get_update_metadata(#riakc_obj{updatemetadata=UM}=Object) ->
    case UM of
        undefined ->
            try
                get_metadata(Object)
            catch 
                throw:no_metadata ->
                    dict:new()
            end;
        UM ->
            UM
    end.
           
%% @doc Return the content type of the update value
get_update_content_type(Object=#riakc_obj{}) ->
    UM = get_update_metadata(Object),
    md_ctype(UM).

%% @doc  Return the updated value of this riakc_obj.
-spec get_update_value(#riakc_obj{}) -> value().
get_update_value(#riakc_obj{updatevalue=UV}=Object) -> 
    case UV of
        undefined ->
            get_value(Object);
        UV ->
            UV
    end.

%% @doc  Return the content type from metadata
-spec md_ctype(dict()) -> undefined | content_type().
md_ctype(MetaData) ->
    case dict:find(?MD_CTYPE, MetaData) of
        error ->
            undefined;
        {ok, Ctype} ->
            Ctype
    end.

%% @doc  Set the vector clock of an object
-spec set_vclock(#riakc_obj{}, vclock()) -> #riakc_obj{}.
set_vclock(Object=#riakc_obj{}, Vclock) ->
    Object#riakc_obj{vclock=Vclock}.

%% @doc  INTERNAL USE ONLY.  Set the contents of riak_object to the
%%       {Metadata, Value} pairs in MVs. Normal clients should use the
%%       set_update_[value|metadata]() + apply_updates() method for changing
%%       object contents.
%% @private
-spec new_obj(bucket(), key(), vclock(), contents()) -> #riakc_obj{}.
new_obj(Bucket, Key, Vclock, Contents) ->
    #riakc_obj{bucket = Bucket, key = Key, vclock = Vclock, contents = Contents}.

%% ===================================================================
%% Unit Tests
%% ===================================================================
-ifdef(TEST).

bucket_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(<<"b">>, bucket(O)).

key_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(<<"k">>, key(O)).

vclock_test() ->
    %% For internal use only
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>, []),
    ?assertEqual(<<"vclock">>, vclock(O)).

newcontent0_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(0, value_count(O)),
    ?assertEqual([], get_metadatas(O)),
    ?assertEqual([], get_values(O)),
    ?assertEqual([], get_contents(O)),
    ?assertThrow(no_metadata, get_metadata(O)),
    ?assertThrow(no_value, get_value(O)).    

contents0_test() ->
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>, []),
    ?assertEqual(0, value_count(O)),
    ?assertEqual([], get_metadatas(O)),
    ?assertEqual([], get_values(O)),
    ?assertEqual([], get_contents(O)),
    ?assertThrow(no_metadata, get_metadata(O)),
    ?assertThrow(no_value, get_value(O)).

contents1_test() ->
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>,
                      [{M1, <<"val1">>}]),
    ?assertEqual(1, value_count(O)),
    ?assertEqual([M1], get_metadatas(O)),
    ?assertEqual([<<"val1">>], get_values(O)),
    ?assertEqual([{M1,<<"val1">>}], get_contents(O)),
    ?assertEqual(M1, get_metadata(O)),
    ?assertEqual(<<"val1">>, get_value(O)).

contents2_test() ->
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    M2 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>,
                      [{M1, <<"val1">>},
                       {M2, <<"val2">>}]),
    ?assertEqual(2, value_count(O)),
    ?assertEqual([M1, M2], get_metadatas(O)),
    ?assertEqual([<<"val1">>, <<"val2">>], get_values(O)),
    ?assertEqual([{M1,<<"val1">>},{M2,<<"val2">>}], get_contents(O)),
    ?assertThrow(siblings, get_metadata(O)),
    ?assertThrow(siblings, get_value(O)).

update_metadata_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    UM = riakc_obj:get_update_metadata(O),
    ?assertEqual([], dict:to_list(UM)).

update_value_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertThrow(no_value, get_update_value(O)),
    O1 = riakc_obj:update_value(O, <<"v">>),
    ?assertEqual(<<"v">>, get_update_value(O1)),
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O2 = riakc_obj:update_metadata(O1, M1),
    ?assertEqual(M1, get_update_metadata(O2)).

updatevalue_ct_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertThrow(no_value, get_update_value(O)),
    O1 = riakc_obj:update_value(O, <<"v">>, "x-application/custom"),
    ?assertEqual(<<"v">>, get_update_value(O1)),
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O2 = riakc_obj:update_metadata(O1, M1),
    ?assertEqual(M1, get_update_metadata(O2)),
    ?assertEqual("x-application/custom", get_update_content_type(O1)).

update_content_type_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    undefined = get_update_content_type(O),
    O1 = update_content_type(O, "application/json"),
    ?assertEqual("application/json", get_update_content_type(O1)).

binary_content_type_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>, <<"v">>, <<"application/x-foo">>),
    ?assertEqual("application/x-foo", get_update_content_type(O)),
    O1 = update_content_type(O, <<"application/x-bar">>),
    ?assertEqual("application/x-bar", get_update_content_type(O1)).

get_update_data_test() ->
    MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
    MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
    O = new_obj(<<"b">>, <<"k">>, <<"">>, 
                [{MD0, <<"v">>}]),
    %% Create an updated metadata object
    Oumd = update_metadata(O, MD1),
    %% Create an updated value object
    Ouv = update_value(O, <<"valueonly">>),
    %% Create updated both object
    Oboth = update_value(Oumd, <<"both">>),

    %% dbgh:start(),
    %% dbgh:trace(?MODULE)
    io:format("O=~p\n", [O]),
    ?assertEqual(<<"v">>, get_update_value(O)),
    MD2 = get_update_metadata(O),
    io:format("MD2=~p\n", [MD2]),
    ?assertEqual("text/plain", md_ctype(MD2)),

    MD3 = get_update_metadata(Oumd),
    ?assertEqual("application/json", md_ctype(MD3)),

    ?assertEqual(<<"valueonly">>, get_update_value(Ouv)),
    MD4 = get_update_metadata(Ouv),
    ?assertEqual("text/plain", md_ctype(MD4)),

    ?assertEqual(<<"both">>, get_update_value(Oboth)),
    MD5 = get_update_metadata(Oboth),
    ?assertEqual("application/json", md_ctype(MD5)).
   
%% get_update_data_sibs_test() ->
%%     MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
%%     MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
%%     O = new_obj(<<"b">>, <<"k">>, <<"">>, 
%%                 [{MD0, <<"v">>},{MD1, <<"sibling">>}]),
%%     %% Create an updated metadata object
%%     Oumd = update_metadata(O, MD1),
%%     %% Create an updated value object
%%     Ouv = update_value(O, <<"valueonly">>),
%%     %% Create updated both object
%%     Oboth = update_value(Oumd, <<"both">>),
    
%%     ?assertThrow({error, siblings}, get_update_data(O)),
%%     ?assertThrow({error, siblings}, get_update_data(Oumd)),
%%     ?assertEqual({error, siblings}, get_update_data(Ouv)),
%%     ?assertEqual({ok, {MD1, <<"both">>}}, get_update_data(Oboth)).
                              
select_sibling_test() ->
    MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
    MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
    O = new_obj(<<"b">>, <<"k">>, <<"">>, 
                [{MD0, <<"sib_one">>},
                 {MD1, <<"sib_two">>}]),
    O1 = select_sibling(1, O),
    O2 = select_sibling(2, O),
    ?assertEqual("text/plain", get_update_content_type(O1)),
    ?assertEqual(<<"sib_one">>, get_update_value(O1)),
    ?assertEqual("application/json", get_update_content_type(O2)),
    ?assertEqual(<<"sib_two">>, get_update_value(O2)).
   
    
    

-endif.
