%%%-------------------------------------------------------------------
%%% File    : etcher_parser.erl
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

-module(etcher_parser).
-export([new/1,
         parse/1,
         parse_until/2,
         skip_until/2,
         compile_variable/2
         ]).

-include("internal.hrl").

-define(IS_VALID_ARG_REQ(T), 
                ((T =:= true) or (T =:= false) or (T =:= optional))).

new(Options) ->
    PS = #ps{tag_db=standard_tags(),
             filter_db=standard_filters()},
    apply_options(Options, PS).

standard_filters() ->
    [check_filter_def(Def) || Def <- etcher_std_filters:standard_filters()].

standard_tags() ->
    [check_tag_def(Def) || Def <- etcher_std_tags:standard_tags()].

check_filter_def(#filter_def{name=Name, mf={Mod, Fun}, arg_required=ArgReq} = F) 
                        when ?IS_STRING(Name), is_atom(Mod), is_atom(Fun), 
                             ?IS_VALID_ARG_REQ(ArgReq) ->
    F;
check_filter_def(BadFilter) ->
    throw({invalid_filter_definition, BadFilter}).

check_tag_def(#tag_def{name=Name, mf={Mod, Fun}} = T) 
                        when ?IS_STRING(Name), is_atom(Mod), is_atom(Fun) ->
    T;
check_tag_def(BadTag) ->
    throw({invalid_tag_definition, BadTag}).

apply_options([{filters, CustomFilters} | Rest], #ps{filter_db=FilterDb} = PS) ->
    case is_list(CustomFilters) of  
        true ->
            CustomFilters1 = [check_filter_def(T) || T <- CustomFilters],
            FilterDb1 = CustomFilters1 ++ FilterDb,
            apply_options(Rest, PS#ps{filter_db=FilterDb1});
        false ->
            throw({bad_filters_option, list_required})
    end;
apply_options([{tags, CustomTags} | Rest], #ps{tag_db=TagDb} = PS) ->
    case is_list(CustomTags) of  
        true ->
            CustomTags1 = [check_tag_def(T) || T <- CustomTags],
            TagDb1 = CustomTags1 ++ TagDb,
            apply_options(Rest, PS#ps{tag_db=TagDb1});
        false ->
            throw({bad_tags_option, list_required})
    end;
apply_options([{template_loaders, TemplateLoaders} | Rest], PS) ->
    apply_options(Rest, PS#ps{template_loaders=TemplateLoaders});
apply_options([BadOpt | _], _PS) ->
    throw({unsupported_parser_option, BadOpt});
apply_options([], PS) ->
    PS.

%%------------------------------------------------------------------------
%% parse 
%%------------------------------------------------------------------------

parse(#ps{} = PS) ->
    ok = check_first_only_tags(PS),
    {end_of_tokens, ParsedData, _NewPS} = parse_until(PS, []),
    {ok, ParsedData}.

check_first_only_tags(#ps{tokens=Tokens, tag_db=TagDb}) ->
    check_first_only_tags(Tokens, TagDb, true).

check_first_only_tags([#tag{name=TagName} | Rest], TagDb, IsFirst) ->
    case lists:keyfind(TagName, #tag_def.name, TagDb) of
        #tag_def{must_be_first=true} when not IsFirst ->
            ErrStr = "Tag '" ++ TagName ++ "' must appear first in template",
            throw({tag_must_be_first, ErrStr});
        _ ->
            check_first_only_tags(Rest, TagDb, false)
    end;
check_first_only_tags([#variable{} | Rest], TagDb, _IsFirst) ->
    check_first_only_tags(Rest, TagDb, false);
check_first_only_tags([_ | Rest], TagDb, IsFirst) ->
    check_first_only_tags(Rest, TagDb, IsFirst);
check_first_only_tags([], _TagDb, _IsFirst) ->
    ok. 

%%------------------------------------------------------------------------
%% TAG API
%%------------------------------------------------------------------------

parse_until(#ps{} = PS, EndTags) when EndTags =:= []; is_list(hd(EndTags)) ->
    do_parse_until(PS, EndTags, []).

% For standard and custom tags which include variables
compile_variable(#ps{} = PS, S) when is_list(S) ->
    case etcher_scanner:scan_var(S) of
        #variable{} = Var ->
            parse_variable(Var, PS)
    end.

%%------------------------------------------------------------------------
%% Parser Core
%%------------------------------------------------------------------------

do_parse_until(#ps{tokens=[#tag{name=TagName} = Tag | Rest]} = PS,
               EndTags,
               Acc) ->
    PS1 = PS#ps{tokens=Rest},
    case lists:member(TagName, EndTags) of
        true ->
            Parsed = lists:reverse(Acc),
            {Tag, Parsed, PS1};
        false ->
            {PS2, Acc1} = parse_tag(Tag, PS1, Acc),
            do_parse_until(PS2, EndTags, Acc1)
    end;
do_parse_until(#ps{tokens=[#variable{} = Var | Rest]} = PS, EndTags, Acc) ->
    Var1 = parse_variable(Var, PS),
    do_parse_until(PS#ps{tokens=Rest}, EndTags, [Var1 | Acc]);
do_parse_until(#ps{tokens=[Tok | Rest]} = PS, EndTags, Acc) ->
    do_parse_until(PS#ps{tokens=Rest}, EndTags, [Tok | Acc]);
do_parse_until(#ps{tokens=[]} = PS, [], Acc) ->
    {end_of_tokens, lists:reverse(Acc), PS};
do_parse_until(#ps{tokens=[]}, EndTags, _Acc) ->
    throw({missing_end_tag, {expected, EndTags}}).

parse_tag(#tag{name=TagName} = Tag, #ps{tag_db=TagDb} = PS, Acc) ->
    case lists:keyfind(TagName, #tag_def.name, TagDb) of
        #tag_def{mf={Mod, Fun}} ->
            case Mod:Fun(PS, Tag) of
                #ps{} = PS1 ->
                    {PS1, Acc};
                {Mfa, #ps{} = PS1} ->
                    Renderer = new_renderer(TagName, Mfa),
                    Acc1 = [Renderer | Acc],
                    {PS1, Acc1}
            end;
        false ->
            throw({no_such_tag, TagName})
    end.

parse_variable(#variable{filters=Filters} = Var, #ps{filter_db=FilterDb}) ->
    Filters1 = parse_filters(Filters, FilterDb, []),
    Filters2 = reorder_escape_filter(Filters1),
    Var#variable{filters=Filters2}.

parse_filters([#filter{name=FilterName, arg=Arg} = Filter | Rest], 
               FilterDb, 
               Acc) ->
    case lists:keyfind(FilterName, #filter_def.name, FilterDb) of
        #filter_def{arg_required=false} when Arg =/= undefined ->
            throw(filter_arg_mismatch(FilterName, 0, 1));
        #filter_def{arg_required=true} when Arg =:= undefined ->
            throw(filter_arg_mismatch(FilterName, 1, 0));
        #filter_def{mf={Mod, Fun}} ->
            Filter1 = Filter#filter{mf={Mod, Fun}},
            parse_filters(Rest, FilterDb, [Filter1 | Acc]);
        false ->
            throw({no_such_filter, FilterName})
    end;
parse_filters([], _FilterDb, Acc) ->
    lists:reverse(Acc).

% No matter where the 'escape' filter appears in the pipeline, it is
% only supposed to take effect after all the other filters have been
% applied. The easiest solution is to just move it to the end of 
% the pipeline (and remove any extraneous duplicates).
reorder_escape_filter(Filters) ->
    IsEscapeFilter = fun(#filter{name=Name}) -> Name =:= "escape" end,
    case lists:partition(IsEscapeFilter, Filters) of
        {[EscapeFilter | _], Filters1} ->
            Filters1 ++ [EscapeFilter];
        {[], Filters} ->
            Filters
    end.

filter_arg_mismatch(FilterName, Required, Provided) ->
    ErrStr = io_lib:format("~s requires ~p arguments, ~p provided", 
                           [FilterName, Required, Provided]),
    {filter_arg_mismatch, lists:flatten(ErrStr)}.

%%------------------------------------------------------------------------
%% skip_until
%%------------------------------------------------------------------------

skip_until(#ps{tokens=[#tag{name=EndTag} | Rest]} = PS, EndTag) ->
    PS#ps{tokens=Rest};
skip_until(#ps{tokens=[_ | Rest]} = PS, EndTag) ->
    skip_until(PS#ps{tokens=Rest}, EndTag);
skip_until(#ps{tokens=[]}, EndTag) ->
    throw({missing_end_tag, {expected, EndTag}}).

%%------------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------------

new_renderer(TagName, {Mod, Fun, _Arg} = Mfa) when is_atom(Mod), is_atom(Fun) ->
    #tag_render{name=TagName, mfa=Mfa}.

