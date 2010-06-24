-module(aleppo_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/local/lib/erlang/lib/parsetools-2.0.2/include/yeccpre.hrl", 0).
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

-spec parse_and_scan({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret().
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
-compile({nowarn_unused_function,{return_error,2}}).
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
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
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

yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

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



-file("src/aleppo/aleppo_parser.erl", 185).

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
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_2(2, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, eof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_2(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_2(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_166(177, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_9_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_166(166, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_89(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_spec(Stack),
 yeccpars2_92(92, 'spec', [89 | Ss], NewStack, T, Ts, Tzr);
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
yeccpars2_90(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'spec', Ss, Stack, T, Ts, Tzr);
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
 'yeccgoto_\'ApplyMacroArg\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ApplyMacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_96_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_!'(Stack),
 yeccpars2_92(92, '!', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_#'(Stack),
 yeccpars2_92(92, '#', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_('(Stack),
 yeccpars2_92(92, '(', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_*'(Stack),
 yeccpars2_92(92, '*', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_+'(Stack),
 yeccpars2_92(92, '+', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_++'(Stack),
 yeccpars2_92(92, '++', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_-'(Stack),
 yeccpars2_92(92, '-', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_--'(Stack),
 yeccpars2_92(92, '--', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_->'(Stack),
 yeccpars2_92(92, '->', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_.'(Stack),
 yeccpars2_92(92, '.', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_/'(Stack),
 yeccpars2_92(92, '/', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_/='(Stack),
 yeccpars2_92(92, '/=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_:'(Stack),
 yeccpars2_92(92, ':', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_:-'(Stack),
 yeccpars2_92(92, ':-', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_::'(Stack),
 yeccpars2_92(92, '::', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_;'(Stack),
 yeccpars2_92(92, ';', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_<'(Stack),
 yeccpars2_92(92, '<', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_<-'(Stack),
 yeccpars2_92(92, '<-', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_<<'(Stack),
 yeccpars2_92(92, '<<', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_<='(Stack),
 yeccpars2_92(92, '<=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_='(Stack),
 yeccpars2_92(92, '=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_=/='(Stack),
 yeccpars2_92(92, '=/=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_=:='(Stack),
 yeccpars2_92(92, '=:=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_=<'(Stack),
 yeccpars2_92(92, '=<', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_=='(Stack),
 yeccpars2_92(92, '==', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_>'(Stack),
 yeccpars2_92(92, '>', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_>='(Stack),
 yeccpars2_92(92, '>=', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_>>'(Stack),
 yeccpars2_92(92, '>>', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_?'(Stack),
 yeccpars2_92(92, '?', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_['(Stack),
 yeccpars2_92(92, '[', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_after(Stack),
 yeccpars2_92(92, 'after', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_and(Stack),
 yeccpars2_92(92, 'and', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_andalso(Stack),
 yeccpars2_92(92, 'andalso', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_atom(Stack),
 yeccpars2_92(92, atom, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_band(Stack),
 yeccpars2_92(92, 'band', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_begin(Stack),
 yeccpars2_92(92, 'begin', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_bnot(Stack),
 yeccpars2_92(92, 'bnot', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_bor(Stack),
 yeccpars2_92(92, 'bor', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_bsl(Stack),
 yeccpars2_92(92, 'bsl', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_bsr(Stack),
 yeccpars2_92(92, 'bsr', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_bxor(Stack),
 yeccpars2_92(92, 'bxor', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_case(Stack),
 yeccpars2_92(92, 'case', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_catch(Stack),
 yeccpars2_92(92, 'catch', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_char(Stack),
 yeccpars2_92(92, char, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_div(Stack),
 yeccpars2_92(92, 'div', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_end(Stack),
 yeccpars2_92(92, 'end', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_float(Stack),
 yeccpars2_92(92, float, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_fun(Stack),
 yeccpars2_92(92, 'fun', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_if(Stack),
 yeccpars2_92(92, 'if', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_integer(Stack),
 yeccpars2_92(92, integer, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_not(Stack),
 yeccpars2_92(92, 'not', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_of(Stack),
 yeccpars2_92(92, 'of', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_or(Stack),
 yeccpars2_92(92, 'or', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_orelse(Stack),
 yeccpars2_92(92, 'orelse', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_query(Stack),
 yeccpars2_92(92, 'query', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_receive(Stack),
 yeccpars2_92(92, 'receive', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_rem(Stack),
 yeccpars2_92(92, 'rem', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_spec(Stack),
 yeccpars2_92(92, 'spec', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_string(Stack),
 yeccpars2_92(92, string, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_try(Stack),
 yeccpars2_92(92, 'try', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_var(Stack),
 yeccpars2_92(92, var, [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_when(Stack),
 yeccpars2_92(92, 'when', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_xor(Stack),
 yeccpars2_92(92, 'xor', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_{'(Stack),
 yeccpars2_92(92, '{', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_|'(Stack),
 yeccpars2_92(92, '|', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_99_||'(Stack),
 yeccpars2_92(92, '||', [99 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccpars2_114(114, Cat, [99 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr).

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
yeccpars2_102(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_spec(Stack),
 yeccpars2_92(92, 'spec', [102 | Ss], NewStack, T, Ts, Tzr);
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
 yeccpars2_110(110, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_103(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_!'(Stack),
 yeccpars2_92(92, '!', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_#'(Stack),
 yeccpars2_92(92, '#', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_('(Stack),
 yeccpars2_92(92, '(', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_*'(Stack),
 yeccpars2_92(92, '*', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_+'(Stack),
 yeccpars2_92(92, '+', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_++'(Stack),
 yeccpars2_92(92, '++', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_-'(Stack),
 yeccpars2_92(92, '-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_--'(Stack),
 yeccpars2_92(92, '--', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_->'(Stack),
 yeccpars2_92(92, '->', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_.'(Stack),
 yeccpars2_92(92, '.', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_/'(Stack),
 yeccpars2_92(92, '/', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_/='(Stack),
 yeccpars2_92(92, '/=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_:'(Stack),
 yeccpars2_92(92, ':', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_:-'(Stack),
 yeccpars2_92(92, ':-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_::'(Stack),
 yeccpars2_92(92, '::', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_;'(Stack),
 yeccpars2_92(92, ';', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<'(Stack),
 yeccpars2_92(92, '<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<-'(Stack),
 yeccpars2_92(92, '<-', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<<'(Stack),
 yeccpars2_92(92, '<<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_<='(Stack),
 yeccpars2_92(92, '<=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_='(Stack),
 yeccpars2_92(92, '=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=/='(Stack),
 yeccpars2_92(92, '=/=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=:='(Stack),
 yeccpars2_92(92, '=:=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=<'(Stack),
 yeccpars2_92(92, '=<', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_=='(Stack),
 yeccpars2_92(92, '==', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>'(Stack),
 yeccpars2_92(92, '>', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>='(Stack),
 yeccpars2_92(92, '>=', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_>>'(Stack),
 yeccpars2_92(92, '>>', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_?'(Stack),
 yeccpars2_92(92, '?', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_['(Stack),
 yeccpars2_92(92, '[', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_after(Stack),
 yeccpars2_92(92, 'after', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_and(Stack),
 yeccpars2_92(92, 'and', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_andalso(Stack),
 yeccpars2_92(92, 'andalso', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_atom(Stack),
 yeccpars2_92(92, atom, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_band(Stack),
 yeccpars2_92(92, 'band', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_begin(Stack),
 yeccpars2_92(92, 'begin', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bnot(Stack),
 yeccpars2_92(92, 'bnot', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bor(Stack),
 yeccpars2_92(92, 'bor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bsl(Stack),
 yeccpars2_92(92, 'bsl', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bsr(Stack),
 yeccpars2_92(92, 'bsr', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bxor(Stack),
 yeccpars2_92(92, 'bxor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_case(Stack),
 yeccpars2_92(92, 'case', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_catch(Stack),
 yeccpars2_92(92, 'catch', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_char(Stack),
 yeccpars2_92(92, char, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_div(Stack),
 yeccpars2_92(92, 'div', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_end(Stack),
 yeccpars2_92(92, 'end', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_float(Stack),
 yeccpars2_92(92, float, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_fun(Stack),
 yeccpars2_92(92, 'fun', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_if(Stack),
 yeccpars2_92(92, 'if', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_integer(Stack),
 yeccpars2_92(92, integer, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_not(Stack),
 yeccpars2_92(92, 'not', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_of(Stack),
 yeccpars2_92(92, 'of', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_or(Stack),
 yeccpars2_92(92, 'or', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_orelse(Stack),
 yeccpars2_92(92, 'orelse', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_query(Stack),
 yeccpars2_92(92, 'query', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_receive(Stack),
 yeccpars2_92(92, 'receive', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_rem(Stack),
 yeccpars2_92(92, 'rem', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_spec(Stack),
 yeccpars2_92(92, 'spec', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_string(Stack),
 yeccpars2_92(92, string, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_try(Stack),
 yeccpars2_92(92, 'try', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_var(Stack),
 yeccpars2_92(92, var, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_when(Stack),
 yeccpars2_92(92, 'when', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_xor(Stack),
 yeccpars2_92(92, 'xor', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_{'(Stack),
 yeccpars2_92(92, '{', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_|'(Stack),
 yeccpars2_92(92, '|', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_103_||'(Stack),
 yeccpars2_92(92, '||', [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccpars2_106(106, Cat, [103 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_104(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'spec', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccpars2_92(92, Cat, [108 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_109(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ':-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'query', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'query', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'spec', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'spec', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_109_(Stack),
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 'yeccgoto_\'MacroString\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_115_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccpars2_92(92, Cat, [116 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_118(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_124: see yeccpars2_44

yeccpars2_125(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_127_(Stack),
 'yeccgoto_\'Undef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_131_(Stack),
 'yeccgoto_\'IncludeLib\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_132(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr).

yeccpars2_134(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_135_(Stack),
 'yeccgoto_\'Include\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_136: see yeccpars2_44

yeccpars2_137(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 'yeccgoto_\'IfNDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_140: see yeccpars2_44

yeccpars2_141(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_143_(Stack),
 'yeccgoto_\'IfDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_144: see yeccpars2_44

yeccpars2_145(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2_157(157, Cat, [146 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_147(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccpars2_149(149, Cat, [148 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_149(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_150_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_152_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_153(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_156(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_159(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccpars2_161(161, Cat, [160 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_161(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_163_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_164(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_165_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_167_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccpars2_174(174, Cat, [168 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_169(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr).

yeccpars2_171(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_172_(Stack),
 'yeccgoto_\'EndIf\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_173_(Stack),
 'yeccgoto_\'Else\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_2(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_177: see yeccpars2_166

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccpars2_174(180, Cat, [179 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_180: see yeccpars2_174

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_181_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'ApplyMacroArg\''(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ApplyMacroArg\''(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ApplyMacroArgs\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Define\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Elements\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(177, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(166, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(168, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(180, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Else\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Else\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'EndIf\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Expression\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(116, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionList\''(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(106, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'ExpressionToken\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'File\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'FormTokens\''(148, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FormTokens\''(160, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IfNDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Include\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'IncludeLib\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Macro\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroArgs\''(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroName\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(125, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(136, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(137, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(141, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'MacroString\''(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyApplyMacroArgs\''(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpression\''(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(116=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyExpressionList\''(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'NonEmptyMacroArgs\''(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(156, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Token\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(149=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

'yeccgoto_\'Undef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(166=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/aleppo/aleppo_parser.yrl", 56).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_3_/1}).
-file("src/aleppo/aleppo_parser.yrl", 59).
yeccpars2_3_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("src/aleppo/aleppo_parser.yrl", 64).
yeccpars2_4_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("src/aleppo/aleppo_parser.yrl", 60).
yeccpars2_5_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("src/aleppo/aleppo_parser.yrl", 63).
yeccpars2_6_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/aleppo/aleppo_parser.yrl", 62).
yeccpars2_7_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("src/aleppo/aleppo_parser.yrl", 56).
yeccpars2_8_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_9_/1}).
-file("src/aleppo/aleppo_parser.yrl", 58).
yeccpars2_9_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/aleppo/aleppo_parser.yrl", 56).
yeccpars2_10_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("src/aleppo/aleppo_parser.yrl", 57).
yeccpars2_11_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/aleppo/aleppo_parser.yrl", 61).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_62_/1}).
-file("src/aleppo/aleppo_parser.yrl", 65).
yeccpars2_62_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("src/aleppo/aleppo_parser.yrl", 54).
yeccpars2_64_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("src/aleppo/aleppo_parser.yrl", 92).
yeccpars2_86_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_89_!'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_#'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_('/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_*'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_+'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_++'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_--'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_->'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_.'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_/'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_:'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_:-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_::'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_;'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_<='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=:='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_=='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_>>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_?'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_['/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_after/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_and/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_andalso/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_atom/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_band/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_begin/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bnot/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bsl/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bsr/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_bxor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_case/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_catch/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_char/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_div/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_end/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_float/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_fun/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_if/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_integer/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_not/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_of/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_or/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_orelse/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_query/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_receive/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_rem/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_spec/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_string/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_try/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_var/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_when/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_xor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_89_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_{'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_|'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_89_||'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_89_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_89_/1}).
-file("src/aleppo/aleppo_parser.yrl", 106).
yeccpars2_89_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_90_/1}).
-file("src/aleppo/aleppo_parser.yrl", 112).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("src/aleppo/aleppo_parser.yrl", 93).
yeccpars2_95_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("src/aleppo/aleppo_parser.yrl", 116).
yeccpars2_96_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/aleppo/aleppo_parser.yrl", 115).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("src/aleppo/aleppo_parser.yrl", 114).
yeccpars2_98_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,'yeccpars2_99_!'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_#'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_('/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_*'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_+'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_++'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_--'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_->'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_.'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_/'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_:'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_:-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_::'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_;'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_<-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_<<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_<='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_=/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_=:='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_=<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_=='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_>='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_>>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_?'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_['/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_after/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_and/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_andalso/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_atom/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_band/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_begin/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_bnot/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_bor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_bsl/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_bsr/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_bxor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_case/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_catch/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_char/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_div/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_end/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_float/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_fun/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_if/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_integer/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_not/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_of/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_or/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_orelse/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_query/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_receive/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_rem/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_spec/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_string/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_try/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_var/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_when/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_xor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_99_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_{'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_|'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_99_||'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_99_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_99_/1}).
-file("src/aleppo/aleppo_parser.yrl", 124).
yeccpars2_99_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_!'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_#'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_('/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_*'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_+'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_++'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_--'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_->'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_.'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_/'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_:'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_:-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_::'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_;'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_<='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=:='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_=='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_>>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_?'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_['/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_after/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_and/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_andalso/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_atom/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_band/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_begin/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bnot/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bsl/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bsr/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_bxor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_case/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_catch/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_char/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_div/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_end/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_float/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_fun/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_if/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_integer/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_not/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_of/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_or/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_orelse/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_query/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_receive/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_rem/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_spec/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_string/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_try/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_var/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_when/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_xor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_102_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_{'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_|'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_||'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_102_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_/1}).
-file("src/aleppo/aleppo_parser.yrl", 124).
yeccpars2_102_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_!'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_!'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_#'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_('/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_*'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_*'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_+'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_++'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_++'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_--'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_--'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_->'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_->'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_.'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_.'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_/'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_/'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_:'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_:'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_:-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_:-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_::'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_;'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<-'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_<-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_<<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_<='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_<='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=/='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_=/='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=:='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_=:='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=<'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_=<'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_=='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_=='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>='/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_>='(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_>>'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_>>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_?'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_?'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_['/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_after/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_after(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_and/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_and(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_andalso/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_andalso(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_atom/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_band/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_band(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_begin/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_begin(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bnot/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_bnot(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_bor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bsl/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_bsl(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bsr/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_bsr(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_bxor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_bxor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_case/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_catch/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_catch(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_char/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_div/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_div(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_end/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_end(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_float/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_fun/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_fun(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_if/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_if(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_integer/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_not/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_not(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_of/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_of(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_or/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_or(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_orelse/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_orelse(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_query/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_query(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_receive/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_receive(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_rem/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_rem(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_spec/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_string/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_try/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_try(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_var/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_when/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_when(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_xor/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_103_xor(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_{'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_|'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_|'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_103_||'/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
'yeccpars2_103_||'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_103_/1}).
-file("src/aleppo/aleppo_parser.yrl", 124).
yeccpars2_103_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_107_/1}).
-file("src/aleppo/aleppo_parser.yrl", 118).
yeccpars2_107_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_108_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_109_/1}).
-file("src/aleppo/aleppo_parser.yrl", 127).
yeccpars2_109_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("src/aleppo/aleppo_parser.yrl", 119).
yeccpars2_111_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("src/aleppo/aleppo_parser.yrl", 95).
yeccpars2_113_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_string , __3 }
  end | __Stack].

-compile({inline,yeccpars2_115_/1}).
-file("src/aleppo/aleppo_parser.yrl", 117).
yeccpars2_115_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("src/aleppo/aleppo_parser.yrl", 121).
yeccpars2_116_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_117_/1}).
-file("src/aleppo/aleppo_parser.yrl", 110).
yeccpars2_117_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("src/aleppo/aleppo_parser.yrl", 90).
yeccpars2_127_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_undef , __4 }
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("src/aleppo/aleppo_parser.yrl", 77).
yeccpars2_131_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include_lib , __4 }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("src/aleppo/aleppo_parser.yrl", 76).
yeccpars2_135_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include , __4 }
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("src/aleppo/aleppo_parser.yrl", 86).
yeccpars2_139_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("src/aleppo/aleppo_parser.yrl", 85).
yeccpars2_143_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-file("src/aleppo/aleppo_parser.yrl", 100).
yeccpars2_146_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_148_/1}).
-file("src/aleppo/aleppo_parser.yrl", 67).
yeccpars2_148_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_/1}).
-file("src/aleppo/aleppo_parser.yrl", 68).
yeccpars2_150_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("src/aleppo/aleppo_parser.yrl", 70).
yeccpars2_151_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("src/aleppo/aleppo_parser.yrl", 69).
yeccpars2_152_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("src/aleppo/aleppo_parser.yrl", 73).
yeccpars2_154_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("src/aleppo/aleppo_parser.yrl", 72).
yeccpars2_155_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("src/aleppo/aleppo_parser.yrl", 104).
yeccpars2_158_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("src/aleppo/aleppo_parser.yrl", 67).
yeccpars2_160_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_163_/1}).
-file("src/aleppo/aleppo_parser.yrl", 74).
yeccpars2_163_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 , __9 }
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("src/aleppo/aleppo_parser.yrl", 103).
yeccpars2_165_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("src/aleppo/aleppo_parser.yrl", 80).
yeccpars2_167_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("src/aleppo/aleppo_parser.yrl", 56).
yeccpars2_168_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_172_/1}).
-file("src/aleppo/aleppo_parser.yrl", 0).
yeccpars2_172_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_173_/1}).
-file("src/aleppo/aleppo_parser.yrl", 0).
yeccpars2_173_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_175_/1}).
-file("src/aleppo/aleppo_parser.yrl", 79).
yeccpars2_175_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("src/aleppo/aleppo_parser.yrl", 83).
yeccpars2_178_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_179_/1}).
-file("src/aleppo/aleppo_parser.yrl", 56).
yeccpars2_179_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_181_/1}).
-file("src/aleppo/aleppo_parser.yrl", 82).
yeccpars2_181_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 , __4 }
  end | __Stack].


