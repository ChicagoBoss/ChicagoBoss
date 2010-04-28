%%%-------------------------------------------------------------------
%%% File    : internal.hrl
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

-include("etcher_api.hrl").

-define(IS_STRING(L), ((L =:= "") orelse is_integer(hd(L)))).

-define(IS_LOWCASE_LETTER(C), ((C >= $a) and (C =< $z))).
-define(IS_UPCASE_LETTER(C), ((C >= $A) and (C =< $Z))).
-define(IS_LETTER(C), (?IS_LOWCASE_LETTER(C) or ?IS_UPCASE_LETTER(C))).
-define(IS_DIGIT(C), ((C >= $0) and (C =< $9))).
-define(IS_ALPHA_NUM(C), (?IS_LETTER(C) or ?IS_DIGIT(C))).

-define(IS_EMPTY_STRING_OBJ(R), (is_record(R, string) and 
                                 (element(#string.val, R) =:= ""))).

-define(EVALS_TO_FALSE(T), (((T =:= false) or
                             (T =:= undefined) or
                             (T =:= 0) or
                             (T =:= "") or 
                             (T =:= <<>>)) 
                            orelse 
                             ?IS_EMPTY_STRING_OBJ(T))).

-record(tag_render, {
            name,               % string()
            mfa                 % {Mod, Fun, Args}
            }).

