%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 2003 Thomas Lindgren <thomasl_erlang@yahoo.com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%%
%%		       SIMPLE MAPPING OVER FORM
%%
%% Author: Thomas Lindgren (030425-)
%%
%% A simplification/specialization of mapform

-module(mapform0).
-author('thomasl_erlang@yahoo.com').
-export([form/2]).

form(F, Form) ->
    case catch map(F, Form) of
	{'EXIT', Rsn} ->
	    io:format("+exited for ~w with ~w~n", [Form, Rsn]),
	    Form;
	NewForm ->
	    NewForm
    end.

%% What we need to do is instrument the proper locations:
%% - clauses
%% - expressions (bifs, ops, exits)
%%
%% Basically, we must avoid confusing patterns and guards with expressions.
%%
%% Note: F only maps over "proper syntax trees", so we do NOT apply
%% the function to constants or lists (nb: all proper syntax tree nodes
%% are tuples)

map(F, {clause, Lc, H, G, B}) ->
    F({clause, Lc, H, G, map(F, B)});
map(F, {match, Lc, P, E}) ->
    F({match, Lc, P, map(F, E)});
map(F, T) when is_tuple(T) ->
    F(list_to_tuple([ map(F, Tsub) || Tsub <- tuple_to_list(T) ]));
map(F, Xs) when is_list(Xs) ->
    [ map(F, X) || X <- Xs ];
map(F, C) when is_atom(C) ; is_number(C) ->
    C.
