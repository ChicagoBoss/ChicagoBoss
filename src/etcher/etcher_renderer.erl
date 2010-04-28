%%%-------------------------------------------------------------------
%%% File    : etcher_renderer.erl
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

-module(etcher_renderer).
-export([new/3,
         render_template/2,
         render_parts/2,
         render_variable/2,
         resolve_variable/2
         ]).

-include("internal.hrl").

-define(IS_FUN(F, Arity), (is_function(F) and is_function(F, Arity))).

new(Context, RecResolver, Options) ->
    Context1 = expand(Context, RecResolver),
    RS = #rs{context=Context1},
    apply_options(Options, RS).

apply_options([{url_mapper, UrlMapper} | Rest], RS) ->
    case ?IS_FUN(UrlMapper, 1) of
        true ->
            RS1 = RS#rs{url_mapper=UrlMapper},
            apply_options(Rest, RS1);
        false ->
            throw({bad_url_mapper, "Must be a fun that takes 1 argument"})
    end;
apply_options([{auto_escape, AutoEsc} | Rest], RS) when is_boolean(AutoEsc) ->
    RS1 = RS#rs{auto_escape=AutoEsc},
    apply_options(Rest, RS1);
apply_options([{allowed_include_roots, Prefixes} | Rest], RS) ->
    IsAbsolutePrefix = fun("/" ++ _) -> true; (_) -> false end,
    case lists:all(IsAbsolutePrefix, Prefixes) of
        true ->
            RS1 = RS#rs{allowed_include_roots=Prefixes},
            apply_options(Rest, RS1);
        false ->
            ErrStr = "Requires a list of absolute file paths",
            throw({bad_allowed_include_roots, ErrStr})
    end;
apply_options([{compiler_opts, CompilerOpts} | Rest], 
              #rs{compiler_opts=CurrentCompilerOpts} = RS) 
                                            when is_list(CompilerOpts) ->
    % We need to be careful here because the template_loaders option 
    % can also modify the #rs.compiler_opts field.
    CompilerOpts1 = proplists:delete(template_loaders, CompilerOpts),
    NewCompilerOpts = CompilerOpts1 ++ CurrentCompilerOpts,
    RS1 = RS#rs{compiler_opts=NewCompilerOpts},
    apply_options(Rest, RS1);
apply_options([{template_loaders, TemplateLoaders} | Rest], 
              #rs{compiler_opts=CompilerOpts} = RS) 
                                            when is_list(TemplateLoaders) ->
    % We store the template loaders with the compiler options in case they
    % are used at compile time (eg: can happen with 'include' tag). The
    % etcher_loader module knows where to look for template loader settings
    % when it needs them.
    CompilerOpts1 = [{template_loaders, TemplateLoaders} | CompilerOpts],
    RS1 = RS#rs{compiler_opts=CompilerOpts1},
    apply_options(Rest, RS1);
apply_options([BadOpt | _], _RS) ->
    throw({unsupported_render_option, BadOpt});
apply_options([], RS) ->
   RS. 

%%------------------------------------------------------------------------
%% TAG API
%%------------------------------------------------------------------------

render_template(#rs{render_trail=RenderTrail} = RS, 
                #etcher_template{version=?CURRENT_TVER, 
                                 md5=TemplateMd5,
                                 content=Parts}) ->
    case lists:member(TemplateMd5, RenderTrail) of
        true ->
            ErrStr = "Templates cannot include themselves, "
                            "directly or indirectly",
            throw({render_loop, ErrStr});
        false ->
            RenderTrail1 = [TemplateMd5 | RenderTrail],
            RS1 = RS#rs{render_trail=RenderTrail1},
            render_parts(RS1, Parts)
    end;
render_template(#rs{}, #etcher_template{version=Ver}) ->
    Err = {template_version_mismatch, 
                {expected_version, ?CURRENT_TVER},
                {received_version, Ver}},
    throw(Err).

render_parts(#rs{} = RS, Parts) ->
    {#rs{globals=Globals}, Rendered} = render(Parts, RS, []),
    RS1 = RS#rs{globals=Globals},
    {RS1, Rendered}.

resolve_variable(RS, #variable{} = Var) ->
    case res_var(Var, RS) of
        #string{val=S} ->
            S;
        Value ->
            Value
    end.

render_variable(RS, #variable{} = Var) ->
    CharData = rend_var(Var, RS#rs{auto_escape=false}),
    unicode:characters_to_list(CharData).

%%------------------------------------------------------------------------
%% Expand Context
%%------------------------------------------------------------------------

expand(Context, RecResolver) ->
    case expand(Context, RecResolver, []) of
        {ok, Context1} ->
            Context1;
        failed ->
            throw({illegal_context, root_proplist_required})
    end.

expand(X, _RecResolver, _Acc) when not is_list(X) ->
    failed;
expand([Tup | Rest], RecResolver, Acc) when is_tuple(Tup) ->
    I = case expand_record(Tup, RecResolver) of
            {ok, PropList} ->
                PropList;
            failed when tuple_size(Tup) =:= 2 ->
                expand_kv(Tup, RecResolver);
            failed ->
                Tup
        end,
    expand(Rest, RecResolver, [I | Acc]);
expand([L | Rest], RecResolver, Acc) when is_list(L) ->
    case expand(L, RecResolver, []) of
        {ok, L1} ->
            expand(Rest, RecResolver, [L1 | Acc]);
        failed ->
            expand(Rest, RecResolver, [L | Acc])
    end;
expand([_ | _Rest], _RecResolver, _Acc) ->
    failed;
expand([], _RecResolver, Acc) ->
    {ok, lists:reverse(Acc)}.

expand_kv({Key, Val}, RecResolver) when is_atom(Key) ->
    Key1 = atom_to_list(Key),
    expand_kv({Key1, Val}, RecResolver);
expand_kv({Key, Val}, RecResolver) ->
    case expand([Val], RecResolver, []) of
        {ok, [Val1]} ->
            {Key, Val1};
        failed ->
            {Key, Val}
    end.

expand_record(_Tup, undefined) ->
    failed;
expand_record(Tup, RecResolver) ->
    RecName = element(1, Tup),
    case dict:find(RecName, RecResolver) of
        {ok, FieldNames} ->
            [RecName | FieldValues] = tuple_to_list(Tup),
            rec_to_proplist(FieldNames, FieldValues, RecResolver, []);
        error ->
            failed
    end.

rec_to_proplist([K | RestKeys], [V | RestVals], RecResolver, Acc) ->
    KV = expand_kv({K, V}, RecResolver),
    rec_to_proplist(RestKeys, RestVals, RecResolver, [KV | Acc]);
rec_to_proplist([], [], _RecResolver, Acc) ->
    {ok, lists:reverse(Acc)};
rec_to_proplist(_, _, _RecResolver, _Acc) ->
    failed.

%%------------------------------------------------------------------------
%% Render Core
%%------------------------------------------------------------------------

render([Text | Rest], RS, Acc) when is_binary(Text) ->
    render(Rest, RS, [Text | Acc]);
render([#variable{} = Var | Rest], RS, Acc) ->
    Data = rend_var(Var, RS),
    render(Rest, RS, [Data | Acc]);
render([#tag_render{mfa={Mod, Fun, Arg}} | Rest], RS, Acc) ->
    case Mod:Fun(RS, Arg) of
        {#rs{context=Context, globals=Globals}, Data} ->
            RS1 = RS#rs{context=Context, globals=Globals},
            render(Rest, RS1, [Data | Acc]);
        Data ->
            render(Rest, RS, [Data | Acc])
    end;
render([], RS, Acc) ->
    {RS, lists:reverse(Acc)}.

rend_var(Var, RS) ->
    Val1 = res_var(Var, RS),
    Val2 = auto_escape(Val1, RS),
    to_iolist(Val2).

auto_escape(Val, #rs{auto_escape=false}) ->
    Val;
auto_escape(#string{} = String, _RS) ->
    etcher_util:conditional_escape(String); 
auto_escape(Val, RS) ->
    String = etcher_util:to_string(Val),
    auto_escape(String, RS).

to_iolist(Val) ->
    try etcher_util:to_iolist(Val)
    catch 
        error:badarg ->
            % Happens when auto_escape is turned off and the variable
            % is a structure containing atoms, etc.
            #string{val=S} = etcher_util:to_string(Val),
            to_iolist(S)
    end.

%%------------------------------------------------------------------------
%% Resolve a variable + filter pipeline
%%------------------------------------------------------------------------

res_var(#variable{filters=Filters} = Var, RS) ->
    Val1 = resolve(Var, RS),
    FilterEscaper = filter_escaper(RS),
    try apply_filters(Filters, FilterEscaper, RS, Val1)
    catch 
        error:badarg -> 
            "";
        error:badarith -> 
            "";
        error:{badmatch, _} -> 
            "";
        error:function_clause -> 
            "";
        error:{case_clause, _} -> 
            "";
        error:if_clause -> 
            "";
        error:{try_clause, _} -> 
            ""
    end.

apply_filters([#filter{mf={Mod, Fun}, arg=Arg} | Rest], Escaper, RS, Val) ->
    Val1 = 
        case Arg of
            undefined ->
                Mod:Fun(Val, Escaper);
            _ ->
                Arg1 = resolve(Arg, RS),
                Mod:Fun(Val, Escaper, Arg1)
        end,
    apply_filters(Rest, Escaper, RS, Val1);
apply_filters([], _Escaper, _RS, Val) ->
    Val.

filter_escaper(#rs{auto_escape=true}) ->
    fun(#string{} = String) ->
        etcher_util:conditional_escape(String)
    end;
filter_escaper(#rs{auto_escape=false}) ->
    fun(#string{} = String) ->
        String
    end.

%%------------------------------------------------------------------------
%% Resolver
%%------------------------------------------------------------------------

resolve(#variable{val=Val}, RS) ->
    resolve(Val, RS);
resolve(Int, _RS) when is_integer(Int) ->
    Int;
resolve(Float, _RS) when is_float(Float) ->
    Float;
resolve(#string{} = String, _RS) ->
    String;
resolve(#ref{parts=KeyPath}, 
        #rs{context=Context, template_string_if_invalid=DefaultString}) ->
    case keypath_lookup(KeyPath, Context) of
        {ok, Val} ->
            Val;
        none ->
            DefaultString
    end.

keypath_lookup([Index], Context) when is_integer(Index) ->
    index_lookup(Index, Context);
keypath_lookup([KeyPart], Context) ->
    case proplists:lookup(KeyPart, Context) of
        {KeyPart, Val} ->
            {ok, Val};
        none ->
            none
    end;
keypath_lookup([Index | RestKey], Context) when is_integer(Index) ->
    case index_lookup(Index, Context) of
        {ok, Context1} when is_list(Context1) ->
            keypath_lookup(RestKey, Context1);
        _ ->
            none
    end;
keypath_lookup([KeyPart | RestKey], Context) ->
    case proplists:get_value(KeyPart, Context) of
        %% Parameterized modules are carried around as tuples.
        Context1 when tuple_size(Context1) >= 2, length(RestKey) =:= 1 ->
            %% As the first stage of the render process the context was 
            %% expanded (see expand/2 above). At this time any {Key,Val}
            %% 2-tuples that were discovered had their Key turned into a 
            %% string. This will also effect paramerized modules which have
            %% just one member. For example, a parameterized module 
            %% defined as "-module(person, [Name]" will be represented as 
            %% the tuple {person,"Name"} which gets expanded to 
            %% {"person","Name"}. In this circumstance "person" needs to 
            %% be changed back to an atom.
            P = case tuple_size(Context1) of
                    2 ->
                        M = list_to_atom(element(1, Context1)),
                        setelement(1, Context1, M);
                    _ ->
                        Context1
                end,
            F = list_to_atom(hd(RestKey)),
            try P:F() of
                Val ->
                    {ok, Val}
            catch
                error:undef ->
                    %% You'll probably want to improve upon this. Ultimately
                    %% it's going to throw an exception, so if you want to 
                    %% handle failures in relation to parameterized modules 
                    %% gracefully, this is a good place to start.
                    keypath_lookup(RestKey, Context1)
            end;
        Context1 when is_list(Context1) ->
            keypath_lookup(RestKey, Context1);
        _ ->
            none
    end.

index_lookup(Index, Context) ->
    IndexStr = integer_to_list(Index),
    case proplists:lookup(IndexStr, Context) of
        {IndexStr, Val} ->
            {ok, Val};
        none ->
            try 
                NonZeroIndex = Index + 1,
                Val = lists:nth(NonZeroIndex, Context),
                {ok, Val}
            catch 
                error:function_clause ->
                    none
            end
    end.

