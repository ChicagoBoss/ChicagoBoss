%%%-------------------------------------------------------------------
%%% File    : etcher_compiler.erl
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

-module(etcher_compiler).
-export([compile/2]).

-include("internal.hrl").

compile(Source, Options) when is_list(Options) ->
    PS = etcher_parser:new(Options),
    compile(Source, PS);
compile(Source, #ps{compile_trail=CompileTrail} = PS) ->
    case unicode:characters_to_list(Source) of
        Unicode when is_list(Unicode) ->
            Md5 = template_md5(Unicode),
            case lists:member(Md5, CompileTrail) of
                true ->
                    ErrStr = "Templates cannot include themselves, "
                                "directly or indirectly",
                    throw({compile_loop, ErrStr});
                false ->
                    CompileTrail1 = [Md5 | CompileTrail],
                    PS1 = PS#ps{compile_trail=CompileTrail1},
                    do_compile(Unicode, Md5, PS1)
            end;
        Err ->
            {error, {unicode, Err}}
    end.

%%------------------------------------------------------------------
%% Misc.
%%------------------------------------------------------------------

do_compile(Source, Md5, PS) ->
    {ok, Tokens} = etcher_scanner:scan(Source),
    PS1 = PS#ps{tokens=Tokens},
    {ok, TemplateContent} = etcher_parser:parse(PS1),
    Template = #etcher_template{
                    md5 = Md5,
                    created = now(),
                    content = TemplateContent},
    {ok, Template}.

template_md5(S) ->
    Utf8Bin = unicode:characters_to_binary(S),
    erlang:md5(Utf8Bin).

