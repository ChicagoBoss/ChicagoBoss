-module(aleppo_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/local/lib/erlang/lib/parsetools-2.0.4/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{{M, F}, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try
        {text, Str} = erl_scan:token_info(Token, text),
        {line, Line} = erl_scan:token_info(Token, line),
        Parts = re:split(Str, "\n"),
        Dline = length(Parts) - 1,
        Yline = Line + Dline,
        case erl_scan:token_info(Token, column) of
            {column, Column} ->
                Col = byte_size(lists:last(Parts)),
                {Yline, Col + if Dline =:= 0 -> Column; true -> 1 end};
            undefined ->
                Yline
        end
    catch _:_ ->
        yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    case catch erl_scan:token_info(Token, text) of
        {text, Txt} -> Txt;
        _ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    case catch erl_scan:token_info(Token, location) of
        {location, Loc} -> Loc;
        _ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:write(Val);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("SRC/aleppo/aleppo_parser.erl", 187).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_2(2, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, eof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 'yeccgoto_\'File\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_3_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_4_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_5_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_6_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_7_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2_165(176, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_9_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_165(165, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_11_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_44(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 'yeccgoto_\'File\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_!'(Stack),
 yeccpars2_92(92, '!', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_#'(Stack),
 yeccpars2_92(92, '#', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_('(Stack),
 yeccpars2_92(92, '(', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_*'(Stack),
 yeccpars2_92(92, '*', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_+'(Stack),
 yeccpars2_92(92, '+', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_++'(Stack),
 yeccpars2_92(92, '++', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_-'(Stack),
 yeccpars2_92(92, '-', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_--'(Stack),
 yeccpars2_92(92, '--', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_->'(Stack),
 yeccpars2_92(92, '->', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_.'(Stack),
 yeccpars2_92(92, '.', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_/'(Stack),
 yeccpars2_92(92, '/', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_/='(Stack),
 yeccpars2_92(92, '/=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_:'(Stack),
 yeccpars2_92(92, ':', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_:-'(Stack),
 yeccpars2_92(92, ':-', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_::'(Stack),
 yeccpars2_92(92, '::', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_;'(Stack),
 yeccpars2_92(92, ';', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_<'(Stack),
 yeccpars2_92(92, '<', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_<-'(Stack),
 yeccpars2_92(92, '<-', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_<<'(Stack),
 yeccpars2_92(92, '<<', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_<='(Stack),
 yeccpars2_92(92, '<=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_='(Stack),
 yeccpars2_92(92, '=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_=/='(Stack),
 yeccpars2_92(92, '=/=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_=:='(Stack),
 yeccpars2_92(92, '=:=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_=<'(Stack),
 yeccpars2_92(92, '=<', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_=='(Stack),
 yeccpars2_92(92, '==', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_>'(Stack),
 yeccpars2_92(92, '>', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_>='(Stack),
 yeccpars2_92(92, '>=', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_>>'(Stack),
 yeccpars2_92(92, '>>', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_?'(Stack),
 yeccpars2_92(92, '?', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_['(Stack),
 yeccpars2_92(92, '[', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_after(Stack),
 yeccpars2_92(92, 'after', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_and(Stack),
 yeccpars2_92(92, 'and', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_andalso(Stack),
 yeccpars2_92(92, 'andalso', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_atom(Stack),
 yeccpars2_92(92, atom, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_band(Stack),
 yeccpars2_92(92, 'band', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_begin(Stack),
 yeccpars2_92(92, 'begin', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_bnot(Stack),
 yeccpars2_92(92, 'bnot', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_bor(Stack),
 yeccpars2_92(92, 'bor', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_bsl(Stack),
 yeccpars2_92(92, 'bsl', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_bsr(Stack),
 yeccpars2_92(92, 'bsr', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_bxor(Stack),
 yeccpars2_92(92, 'bxor', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_case(Stack),
 yeccpars2_92(92, 'case', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_catch(Stack),
 yeccpars2_92(92, 'catch', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_char(Stack),
 yeccpars2_92(92, char, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_div(Stack),
 yeccpars2_92(92, 'div', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_end(Stack),
 yeccpars2_92(92, 'end', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_float(Stack),
 yeccpars2_92(92, float, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_fun(Stack),
 yeccpars2_92(92, 'fun', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_if(Stack),
 yeccpars2_92(92, 'if', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_integer(Stack),
 yeccpars2_92(92, integer, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_not(Stack),
 yeccpars2_92(92, 'not', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_of(Stack),
 yeccpars2_92(92, 'of', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_or(Stack),
 yeccpars2_92(92, 'or', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_orelse(Stack),
 yeccpars2_92(92, 'orelse', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_query(Stack),
 yeccpars2_92(92, 'query', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_receive(Stack),
 yeccpars2_92(92, 'receive', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_rem(Stack),
 yeccpars2_92(92, 'rem', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_spec(Stack),
 yeccpars2_92(92, spec, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_string(Stack),
 yeccpars2_92(92, string, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_try(Stack),
 yeccpars2_92(92, 'try', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_var(Stack),
 yeccpars2_92(92, var, [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_when(Stack),
 yeccpars2_92(92, 'when', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_xor(Stack),
 yeccpars2_92(92, 'xor', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_{'(Stack),
 yeccpars2_92(92, '{', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_|'(Stack),
 yeccpars2_92(92, '|', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_89_||'(Stack),
 yeccpars2_92(92, '||', [89 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_(Stack),
 yeccpars2_93(93, Cat, [89 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_90(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ApplyMacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_92(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_92(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_92(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_93(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_!'(Stack),
 yeccpars2_92(92, '!', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_#'(Stack),
 yeccpars2_92(92, '#', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_('(Stack),
 yeccpars2_92(92, '(', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_*'(Stack),
 yeccpars2_92(92, '*', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_+'(Stack),
 yeccpars2_92(92, '+', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_++'(Stack),
 yeccpars2_92(92, '++', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_-'(Stack),
 yeccpars2_92(92, '-', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_--'(Stack),
 yeccpars2_92(92, '--', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_->'(Stack),
 yeccpars2_92(92, '->', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_.'(Stack),
 yeccpars2_92(92, '.', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_/'(Stack),
 yeccpars2_92(92, '/', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_/='(Stack),
 yeccpars2_92(92, '/=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_:'(Stack),
 yeccpars2_92(92, ':', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_:-'(Stack),
 yeccpars2_92(92, ':-', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_::'(Stack),
 yeccpars2_92(92, '::', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_;'(Stack),
 yeccpars2_92(92, ';', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_<'(Stack),
 yeccpars2_92(92, '<', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_<-'(Stack),
 yeccpars2_92(92, '<-', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_<<'(Stack),
 yeccpars2_92(92, '<<', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_<='(Stack),
 yeccpars2_92(92, '<=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_='(Stack),
 yeccpars2_92(92, '=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_=/='(Stack),
 yeccpars2_92(92, '=/=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_=:='(Stack),
 yeccpars2_92(92, '=:=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_=<'(Stack),
 yeccpars2_92(92, '=<', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_=='(Stack),
 yeccpars2_92(92, '==', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_>'(Stack),
 yeccpars2_92(92, '>', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_>='(Stack),
 yeccpars2_92(92, '>=', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_>>'(Stack),
 yeccpars2_92(92, '>>', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_?'(Stack),
 yeccpars2_92(92, '?', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_['(Stack),
 yeccpars2_92(92, '[', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_after(Stack),
 yeccpars2_92(92, 'after', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_and(Stack),
 yeccpars2_92(92, 'and', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_andalso(Stack),
 yeccpars2_92(92, 'andalso', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_atom(Stack),
 yeccpars2_92(92, atom, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_band(Stack),
 yeccpars2_92(92, 'band', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_begin(Stack),
 yeccpars2_92(92, 'begin', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_bnot(Stack),
 yeccpars2_92(92, 'bnot', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_bor(Stack),
 yeccpars2_92(92, 'bor', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_bsl(Stack),
 yeccpars2_92(92, 'bsl', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_bsr(Stack),
 yeccpars2_92(92, 'bsr', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_bxor(Stack),
 yeccpars2_92(92, 'bxor', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_case(Stack),
 yeccpars2_92(92, 'case', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_catch(Stack),
 yeccpars2_92(92, 'catch', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_char(Stack),
 yeccpars2_92(92, char, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_div(Stack),
 yeccpars2_92(92, 'div', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_end(Stack),
 yeccpars2_92(92, 'end', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_float(Stack),
 yeccpars2_92(92, float, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_fun(Stack),
 yeccpars2_92(92, 'fun', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_if(Stack),
 yeccpars2_92(92, 'if', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_integer(Stack),
 yeccpars2_92(92, integer, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_not(Stack),
 yeccpars2_92(92, 'not', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_of(Stack),
 yeccpars2_92(92, 'of', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_or(Stack),
 yeccpars2_92(92, 'or', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_orelse(Stack),
 yeccpars2_92(92, 'orelse', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_query(Stack),
 yeccpars2_92(92, 'query', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_receive(Stack),
 yeccpars2_92(92, 'receive', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_rem(Stack),
 yeccpars2_92(92, 'rem', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_spec(Stack),
 yeccpars2_92(92, spec, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_string(Stack),
 yeccpars2_92(92, string, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_try(Stack),
 yeccpars2_92(92, 'try', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_var(Stack),
 yeccpars2_92(92, var, [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_when(Stack),
 yeccpars2_92(92, 'when', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_xor(Stack),
 yeccpars2_92(92, 'xor', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_{'(Stack),
 yeccpars2_92(92, '{', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_|'(Stack),
 yeccpars2_92(92, '|', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_98_||'(Stack),
 yeccpars2_92(92, '||', [98 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccpars2_113(113, Cat, [98 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_!'(Stack),
 yeccpars2_92(92, '!', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_#'(Stack),
 yeccpars2_92(92, '#', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_('(Stack),
 yeccpars2_92(92, '(', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_*'(Stack),
 yeccpars2_92(92, '*', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_+'(Stack),
 yeccpars2_92(92, '+', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_++'(Stack),
 yeccpars2_92(92, '++', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_-'(Stack),
 yeccpars2_92(92, '-', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_--'(Stack),
 yeccpars2_92(92, '--', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_->'(Stack),
 yeccpars2_92(92, '->', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_.'(Stack),
 yeccpars2_92(92, '.', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_/'(Stack),
 yeccpars2_92(92, '/', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_/='(Stack),
 yeccpars2_92(92, '/=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_:'(Stack),
 yeccpars2_92(92, ':', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_:-'(Stack),
 yeccpars2_92(92, ':-', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_::'(Stack),
 yeccpars2_92(92, '::', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_;'(Stack),
 yeccpars2_92(92, ';', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_<'(Stack),
 yeccpars2_92(92, '<', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_<-'(Stack),
 yeccpars2_92(92, '<-', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_<<'(Stack),
 yeccpars2_92(92, '<<', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_<='(Stack),
 yeccpars2_92(92, '<=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_='(Stack),
 yeccpars2_92(92, '=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_=/='(Stack),
 yeccpars2_92(92, '=/=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_=:='(Stack),
 yeccpars2_92(92, '=:=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_=<'(Stack),
 yeccpars2_92(92, '=<', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_=='(Stack),
 yeccpars2_92(92, '==', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_>'(Stack),
 yeccpars2_92(92, '>', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_>='(Stack),
 yeccpars2_92(92, '>=', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_>>'(Stack),
 yeccpars2_92(92, '>>', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_?'(Stack),
 yeccpars2_92(92, '?', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_['(Stack),
 yeccpars2_92(92, '[', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_after(Stack),
 yeccpars2_92(92, 'after', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_and(Stack),
 yeccpars2_92(92, 'and', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_andalso(Stack),
 yeccpars2_92(92, 'andalso', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_atom(Stack),
 yeccpars2_92(92, atom, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_band(Stack),
 yeccpars2_92(92, 'band', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_begin(Stack),
 yeccpars2_92(92, 'begin', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_bnot(Stack),
 yeccpars2_92(92, 'bnot', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_bor(Stack),
 yeccpars2_92(92, 'bor', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_bsl(Stack),
 yeccpars2_92(92, 'bsl', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_bsr(Stack),
 yeccpars2_92(92, 'bsr', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_bxor(Stack),
 yeccpars2_92(92, 'bxor', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_case(Stack),
 yeccpars2_92(92, 'case', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_catch(Stack),
 yeccpars2_92(92, 'catch', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_char(Stack),
 yeccpars2_92(92, char, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_div(Stack),
 yeccpars2_92(92, 'div', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_end(Stack),
 yeccpars2_92(92, 'end', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_float(Stack),
 yeccpars2_92(92, float, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_fun(Stack),
 yeccpars2_92(92, 'fun', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_if(Stack),
 yeccpars2_92(92, 'if', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_integer(Stack),
 yeccpars2_92(92, integer, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_not(Stack),
 yeccpars2_92(92, 'not', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_of(Stack),
 yeccpars2_92(92, 'of', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_or(Stack),
 yeccpars2_92(92, 'or', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_orelse(Stack),
 yeccpars2_92(92, 'orelse', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_query(Stack),
 yeccpars2_92(92, 'query', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_receive(Stack),
 yeccpars2_92(92, 'receive', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_rem(Stack),
 yeccpars2_92(92, 'rem', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_spec(Stack),
 yeccpars2_92(92, spec, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_string(Stack),
 yeccpars2_92(92, string, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_try(Stack),
 yeccpars2_92(92, 'try', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_var(Stack),
 yeccpars2_92(92, var, [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_when(Stack),
 yeccpars2_92(92, 'when', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_xor(Stack),
 yeccpars2_92(92, 'xor', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_{'(Stack),
 yeccpars2_92(92, '{', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_|'(Stack),
 yeccpars2_92(92, '|', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_101_||'(Stack),
 yeccpars2_92(92, '||', [101 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccpars2_109(109, Cat, [101 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_102(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_!'(Stack),
 yeccpars2_92(92, '!', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_#'(Stack),
 yeccpars2_92(92, '#', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_('(Stack),
 yeccpars2_92(92, '(', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_*'(Stack),
 yeccpars2_92(92, '*', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_+'(Stack),
 yeccpars2_92(92, '+', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_++'(Stack),
 yeccpars2_92(92, '++', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_-'(Stack),
 yeccpars2_92(92, '-', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_--'(Stack),
 yeccpars2_92(92, '--', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_->'(Stack),
 yeccpars2_92(92, '->', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_.'(Stack),
 yeccpars2_92(92, '.', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_/'(Stack),
 yeccpars2_92(92, '/', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_/='(Stack),
 yeccpars2_92(92, '/=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_:'(Stack),
 yeccpars2_92(92, ':', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_:-'(Stack),
 yeccpars2_92(92, ':-', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_::'(Stack),
 yeccpars2_92(92, '::', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_;'(Stack),
 yeccpars2_92(92, ';', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_<'(Stack),
 yeccpars2_92(92, '<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_<-'(Stack),
 yeccpars2_92(92, '<-', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_<<'(Stack),
 yeccpars2_92(92, '<<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_<='(Stack),
 yeccpars2_92(92, '<=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_='(Stack),
 yeccpars2_92(92, '=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_=/='(Stack),
 yeccpars2_92(92, '=/=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_=:='(Stack),
 yeccpars2_92(92, '=:=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_=<'(Stack),
 yeccpars2_92(92, '=<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_=='(Stack),
 yeccpars2_92(92, '==', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_>'(Stack),
 yeccpars2_92(92, '>', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_>='(Stack),
 yeccpars2_92(92, '>=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_>>'(Stack),
 yeccpars2_92(92, '>>', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_?'(Stack),
 yeccpars2_92(92, '?', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_['(Stack),
 yeccpars2_92(92, '[', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_after(Stack),
 yeccpars2_92(92, 'after', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_and(Stack),
 yeccpars2_92(92, 'and', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_andalso(Stack),
 yeccpars2_92(92, 'andalso', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_atom(Stack),
 yeccpars2_92(92, atom, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_band(Stack),
 yeccpars2_92(92, 'band', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_begin(Stack),
 yeccpars2_92(92, 'begin', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_bnot(Stack),
 yeccpars2_92(92, 'bnot', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_bor(Stack),
 yeccpars2_92(92, 'bor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_bsl(Stack),
 yeccpars2_92(92, 'bsl', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_bsr(Stack),
 yeccpars2_92(92, 'bsr', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_bxor(Stack),
 yeccpars2_92(92, 'bxor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_case(Stack),
 yeccpars2_92(92, 'case', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_catch(Stack),
 yeccpars2_92(92, 'catch', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_char(Stack),
 yeccpars2_92(92, char, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_div(Stack),
 yeccpars2_92(92, 'div', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_end(Stack),
 yeccpars2_92(92, 'end', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_float(Stack),
 yeccpars2_92(92, float, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_fun(Stack),
 yeccpars2_92(92, 'fun', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_if(Stack),
 yeccpars2_92(92, 'if', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_integer(Stack),
 yeccpars2_92(92, integer, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_not(Stack),
 yeccpars2_92(92, 'not', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_of(Stack),
 yeccpars2_92(92, 'of', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_or(Stack),
 yeccpars2_92(92, 'or', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_orelse(Stack),
 yeccpars2_92(92, 'orelse', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_query(Stack),
 yeccpars2_92(92, 'query', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_receive(Stack),
 yeccpars2_92(92, 'receive', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_rem(Stack),
 yeccpars2_92(92, 'rem', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_spec(Stack),
 yeccpars2_92(92, spec, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_string(Stack),
 yeccpars2_92(92, string, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_try(Stack),
 yeccpars2_92(92, 'try', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_var(Stack),
 yeccpars2_92(92, var, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_when(Stack),
 yeccpars2_92(92, 'when', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_xor(Stack),
 yeccpars2_92(92, 'xor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_{'(Stack),
 yeccpars2_92(92, '{', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_|'(Stack),
 yeccpars2_92(92, '|', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_||'(Stack),
 yeccpars2_92(92, '||', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccpars2_105(105, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_103(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2_92(92, Cat, [107 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_108(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 'yeccgoto_\'MacroString\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccpars2_92(92, Cat, [115 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_116(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_118(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_119(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_120(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_121(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_122(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_123: see yeccpars2_44

yeccpars2_124(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_125(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_126_(Stack),
 'yeccgoto_\'Undef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_128(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_129(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 'yeccgoto_\'IncludeLib\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_131(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_132(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_133(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 'yeccgoto_\'Include\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_135: see yeccpars2_44

yeccpars2_136(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_137(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 'yeccgoto_\'IfNDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_139: see yeccpars2_44

yeccpars2_140(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_141(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 'yeccgoto_\'IfDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_44

yeccpars2_144(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_145(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccpars2_156(156, Cat, [145 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_146(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccpars2_148(148, Cat, [147 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_148(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_92(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_149_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_153_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_158(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_158(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 yeccpars2_160(160, Cat, [159 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_160(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_92(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_162_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_163(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_92(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccpars2_173(173, Cat, [167 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_168(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_169(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_170(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_170(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_171_(Stack),
 'yeccgoto_\'EndIf\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_(Stack),
 'yeccgoto_\'Else\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_173(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_92(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_174_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_175(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_176: see yeccpars2_165

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_177_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccpars2_173(179, Cat, [178 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_179: see yeccpars2_173

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_180_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'ApplyMacroArgs\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Define\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Elements\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(176, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(173, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(179, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Else\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Else\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'EndIf\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Expression\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionList\''(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionToken\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(160=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'File\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'FormTokens\''(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FormTokens\''(159, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Include\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IncludeLib\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Macro\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(160=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroArgs\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroName\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(123, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroString\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(160=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyApplyMacroArgs\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpression\''(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(98=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpressionList\''(98, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyMacroArgs\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Token\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(160=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Undef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 56).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_2_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 54).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ { eof , 0 } ]
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 59).
yeccpars2_3_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 64).
yeccpars2_4_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 60).
yeccpars2_5_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 63).
yeccpars2_6_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 62).
yeccpars2_7_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 56).
yeccpars2_8_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_9_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 58).
yeccpars2_9_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 56).
yeccpars2_10_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 57).
yeccpars2_11_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 61).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 65).
yeccpars2_62_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 53).
yeccpars2_64_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 92).
yeccpars2_86_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_89_!'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_#'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_('/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_*'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_+'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_++'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_--'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_->'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_.'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_/'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_:'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_:-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_::'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_;'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=:='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_?'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_['/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_after/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_and/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_andalso/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_atom/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_band/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_begin/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bnot/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bsl/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bsr/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bxor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_case/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_catch/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_char/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_div/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_end/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_float/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_fun/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_if/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_integer/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_not/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_of/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_or/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_orelse/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_query/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_receive/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_rem/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_spec/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_string/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_try/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_var/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_when/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_xor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_89_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_{'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_|'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_||'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_89_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 106).
yeccpars2_89_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_90_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 109).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 93).
yeccpars2_94_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 114).
yeccpars2_95_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 113).
yeccpars2_96_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 112).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,'yeccpars2_98_!'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_#'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_('/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_*'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_+'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_++'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_--'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_->'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_.'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_/'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_:'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_:-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_::'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_;'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_<-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_<<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_<='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_=/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_=:='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_=<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_=='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_>='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_>>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_?'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_['/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_after/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_and/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_andalso/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_atom/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_band/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_begin/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_bnot/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_bor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_bsl/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_bsr/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_bxor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_case/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_catch/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_char/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_div/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_end/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_float/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_fun/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_if/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_integer/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_not/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_of/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_or/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_orelse/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_query/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_receive/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_rem/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_spec/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_string/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_try/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_var/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_when/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_xor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_98_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_{'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_|'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_98_||'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_98_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 122).
yeccpars2_98_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_!'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_#'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_('/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_*'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_+'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_++'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_--'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_->'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_.'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_/'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_:'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_:-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_::'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_;'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_<-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_<<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_<='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_=/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_=:='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_=<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_=='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_>='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_>>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_?'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_['/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_after/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_and/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_andalso/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_atom/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_band/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_begin/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_bnot/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_bor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_bsl/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_bsr/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_bxor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_case/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_catch/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_char/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_div/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_end/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_float/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_fun/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_if/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_integer/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_not/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_of/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_or/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_orelse/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_query/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_receive/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_rem/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_spec/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_string/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_try/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_var/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_when/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_xor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_101_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_{'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_|'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_101_||'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_101_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_101_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 122).
yeccpars2_101_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_!'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_#'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_('/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_*'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_+'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_++'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_--'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_->'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_.'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_/'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_:'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_:-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_::'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_;'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<-'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=/='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=:='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=<'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>='/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>>'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_?'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_['/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_after/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_and/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_andalso/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_atom/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_band/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_begin/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bnot/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bsl/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bsr/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bxor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_case/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_catch/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_char/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_div/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_end/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_float/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_fun/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_if/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_integer/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_not/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_of/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_or/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_orelse/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_query/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_receive/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_rem/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_spec/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_string/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_try/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_var/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_when/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_xor/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_102_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_{'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_|'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_||'/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
'yeccpars2_102_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 122).
yeccpars2_102_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 116).
yeccpars2_106_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_107_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_108_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 125).
yeccpars2_108_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_110_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 117).
yeccpars2_110_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_112_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 95).
yeccpars2_112_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_string , __3 }
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 115).
yeccpars2_114_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 119).
yeccpars2_115_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_116_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 110).
yeccpars2_116_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 90).
yeccpars2_126_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_undef , __4 }
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 77).
yeccpars2_130_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include_lib , __4 }
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 76).
yeccpars2_134_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include , __4 }
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 86).
yeccpars2_138_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 85).
yeccpars2_142_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 100).
yeccpars2_145_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_147_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 67).
yeccpars2_147_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_149_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 68).
yeccpars2_149_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_150_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 70).
yeccpars2_150_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 69).
yeccpars2_151_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 73).
yeccpars2_153_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 72).
yeccpars2_154_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 }
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 104).
yeccpars2_157_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ [ __1 ] ]
  end | __Stack].

-compile({inline,yeccpars2_159_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 67).
yeccpars2_159_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_162_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 74).
yeccpars2_162_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 , __9 }
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 103).
yeccpars2_164_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ [ __3 ] ]
  end | __Stack].

-compile({inline,yeccpars2_166_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 80).
yeccpars2_166_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 56).
yeccpars2_167_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_171_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 0).
yeccpars2_171_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 0).
yeccpars2_172_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_174_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 79).
yeccpars2_174_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 83).
yeccpars2_177_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 56).
yeccpars2_178_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_180_/1}).
-file("SRC/aleppo/aleppo_parser.yrl", 82).
yeccpars2_180_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 , __4 }
  end | __Stack].


