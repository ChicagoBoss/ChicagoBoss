%%%-------------------------------------------------------------------
%%% File    : etcher_rec_resolver.erl
%%% Project : Etcher (http://github.com/jinsky/etcher)
%%% Author  : Rory Byrne <rory [at] jinsky [dot] com>
%%% License : BSD
%%%
%%% Copyright (c) 2010 Rory Byrne
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-module(etcher_rec_resolver).
-export([get_record_resolver/0,
         get_record_resolver/1,
         add_record_defs/1,
         add_record_defs/2
         ]).

-include("internal.hrl").

-define(DEFAULT_RR_NAMESPACE, global).

get_record_resolver() ->
    get_record_resolver(?DEFAULT_RR_NAMESPACE).

get_record_resolver(Namespace) ->
    Key = record_resolver_key(Namespace),
    case db_lookup(Key) of
        {ok, Resolver} ->
            Resolver;
        undefined ->
            undefined
    end.

%%-----------------------------------------------------------------------
%% Record Resolver Management.
%%-----------------------------------------------------------------------

add_record_defs(T) ->
    add_record_defs(T, ?DEFAULT_RR_NAMESPACE).

add_record_defs([{_, _} | _] = RecDefs, Namespace) when is_atom(Namespace) ->
    RecDefs1 = prep_record_defs(RecDefs, []),
    save_record_defs(Namespace, RecDefs1);
add_record_defs(FileName, Namespace) when is_binary(FileName) ->
    add_record_defs(binary_to_list(FileName), Namespace);
add_record_defs(FileName, Namespace) when is_list(FileName) ->
    case parse_records_from(FileName) of
        {error, _Reason} = Err ->
            Err;
        {ok, []} ->
            {error, no_record_definitions_in_file};
        {ok, RecDefs} ->
            add_record_defs(RecDefs, Namespace)
    end.

parse_records_from(FileName) ->
    case epp:parse_file(FileName, [], []) of
        {ok, AbsForms} ->
             extract_record_defs(AbsForms, []);
        {error, _Reason} = Err ->
            Err
    end.

extract_record_defs([{error, E} | _], _Acc) ->
    {error, {parse_error, E}};
extract_record_defs([{attribute, _, record, {RecName, AbsFields}} | Rest], 
                    Acc) ->
    FieldNames = 
        [begin 
            {atom, _, Name} = element(3, T), 
            Name 
         end || T <- AbsFields],
    extract_record_defs(Rest, [{RecName, FieldNames} | Acc]);
extract_record_defs([_ | Rest], Acc) ->
    extract_record_defs(Rest, Acc);
extract_record_defs([], Acc) ->
    {ok, Acc}.

prep_record_defs([{RecName, FieldNames} = Def | Rest], Acc) 
                                            when is_list(FieldNames) ->
    case lists:all(fun(A) -> is_atom(A) end, [RecName | FieldNames]) of
        true ->
            ok;
        false ->
            throw({bad_arg, {{expected, "{atom(), [atom()]}"}, 
                             {received, Def}}})
    end,
    StrFieldNames = [atom_to_list(A) || A <- FieldNames],
    Def1 = {RecName, StrFieldNames},
    prep_record_defs(Rest, [Def1 | Acc]);
prep_record_defs([], Acc) ->
    lists:reverse(Acc).

save_record_defs(Namespace, RecDefs) ->
    F = fun(undefined) ->
               dict:from_list(RecDefs);
           (Dict) ->
               DoUpdate = fun({K, V}, D) -> dict:store(K, V, D) end,
               lists:foldl(DoUpdate, Dict, RecDefs)
        end,
    Key = record_resolver_key(Namespace), 
    db_update(Key, F).

%%-----------------------------------------------------------------------
%% DB
%%-----------------------------------------------------------------------

db_lookup(Key) ->
    application:get_env(this_application(), Key).

db_store({Key, Val}) ->
    application:set_env(this_application(), Key, Val).

db_update(Key, DoUpdate) ->
    CurrentVal = 
        case db_lookup(Key) of
            {ok, Val} ->
                Val;
            undefined ->
                undefined
        end,
    NewVal = DoUpdate(CurrentVal),
    db_store({Key, NewVal}).

%%-----------------------------------------------------------------------
%% Misc.
%%-----------------------------------------------------------------------

record_resolver_key(Namespace) ->
    {?MODULE, Namespace}.
 
this_application() ->
    case application:get_application(?MODULE) of
        {ok, App} ->
            App;
        undefined ->
            throw({application_not_started, 
                   "Need to do application:start(etcher) first"})
    end.

