%%%-------------------------------------------------------------------
%%% File    : etcher_loader.erl
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

-module(etcher_loader).
-export([get_template/2,
         file_loader/2
         ]).

-include("internal.hrl").

get_template(#ps{template_loaders=TemplateLoaders} = PS, Path) ->
    load_and_compile(TemplateLoaders, Path, PS);
get_template(#rs{compiler_opts=CompilerOpts}, Path) ->
    TemplateLoaders = proplists:get_value(template_loaders, CompilerOpts, []),
    load_and_compile(TemplateLoaders, Path, CompilerOpts).

load_and_compile(TemplateLoaders, Path, CompilerArg) ->
    case load_template(TemplateLoaders, Path) of
        #etcher_template{} = Tpl ->
            Tpl;
        undefined ->
            undefined;
        Bin when is_binary(Bin) ->
            try binary_to_term(Bin) of
                #etcher_template{} = Tpl ->
                    Tpl;
                BadTerm ->
                    {error, {non_template_term, BadTerm}}
            catch
                error:badarg ->
                    compile_template(Bin, CompilerArg)
            end;
        CharData when is_list(CharData) ->
            compile_template(CharData, CompilerArg)
    end.

compile_template(CharData, CompilerArg) ->
    {ok, Tpl} = etcher_compiler:compile(CharData, CompilerArg),
    Tpl.

load_template([{file, Args} | Rest], Path) ->
    load_template([{?MODULE, file_loader, Args} | Rest], Path);
load_template([{Mod, Fun, Args} | Rest], Path) when is_list(Args) ->
    case Mod:Fun(Path, Args) of
        undefined ->
            load_template(Rest, Path);
        T ->
            T
    end;
load_template([], _Path) ->
    undefined.

%%------------------------------------------------------------------------
%% Template File Loader
%%------------------------------------------------------------------------

file_loader(Path, TemplateDirs) ->
    case etcher_util:normalize_relative_path(Path) of
        illegal ->
            undefined;
        Path1 ->
            find_file(TemplateDirs, Path1)
    end.

find_file(["/" ++ _ = Dir | Rest], Path) ->
    FileName = filename:join(Dir, Path),
    case file:read_file(FileName) of
        {ok, Bin} ->
            Bin;
        {error, _} ->
            find_file(Rest, Path)
    end;
find_file([_ | Rest], Path) ->
    find_file(Rest, Path);                  % Ignore non-absolute template dirs
find_file([], _Path) ->
    undefined.

