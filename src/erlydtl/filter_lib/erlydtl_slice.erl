 -module(erlydtl_slice).

-export([slice/2,slice_input_cases/7]).

-define(NOTEST,1).
% remark out NODEBUG when running tests; unremark when debugging indivdual use cases
-define(NODEBUG,1).
-include_lib("eunit/include/eunit.hrl").

slice(List,":") ->
    List;
slice(List,Index) ->
    ListLength = erlang:length(List),
    {Start,End,C1,C2,Step} = parse_index(Index),
    try
      slice_input_cases(List,ListLength,Start,C1,End,C2,Step)
    catch
      throw:outOfBounds ->
	  [];
      throw:indexError ->
	  indexError
    end.

slice_input_cases(_List,ListLength,Start,false,[],false,[]) when Start > 0, Start >= ListLength ->
    throw(indexError);
slice_input_cases(_List,ListLength,Start,false,[],false,[]) when Start < 0, Start < -ListLength ->
    throw(indexError);
%[-1]
slice_input_cases(List,ListLength,Start,false,[],false,[]) when Start<0 ->
    S = start_transform(ListLength,Start+ListLength+1),
    LowerBound = single_index_bounds(S),
    ?debugFmt("slice_transform exit: ~p, ~p, ~p~n",[List,S,LowerBound]),
    %[Result] = lists:sublist(List,LowerBound,Step),
    lists:nth(LowerBound,List);
%[1]
slice_input_cases(List,ListLength,Start,false,[],false,[]) ->
    %S = start_transform(ListLength,Start+1),
    %E = end_transform(ListLength,Start+2),
    Step = 1,
    End = Start + 1,
    {Start1,End1,Step1} = index_defaults(ListLength,Start,End,Step),
    S = start_transform(ListLength,Start1),
    E = end_transform(ListLength,End1),
    ?debugFmt("slice_transform: S,E,Step1: ~p,~p,~p~n",[S,E,Step1]),
    [Result] = slice_list(List,ListLength,S,false,E,false,Step1),
    Result;
%slice_transform(List, ListLength, Start, C1, End, C2, Step) when End < 0, Step > 0 ->
%    [];
%slice_transform(List, ListLength, Start, C1, End, C2, Step) when End > 0, Step < 0 ->
%    [];
%[::-1]
slice_input_cases(List,ListLength,[],true,[],true,Step) when Step < 0 ->
    ?debugMsg("here"),
    slice_transform(List,ListLength,ListLength,true,-2*(ListLength+1),true,Step);
%[::1]
slice_input_cases(List,ListLength,[],true,[],true,Step) when Step > 0 ->
    slice_transform(List,ListLength,0,true,ListLength,true,Step);
slice_input_cases(List,ListLength,Start,C1,End,C2,Step) ->
    slice_transform(List,ListLength,Start,C1,End,C2,Step).

%[N:N:N]
slice_transform(List,ListLength,Start,C1,End,C2,Step) ->
    {Start1,End1,Step1} = index_defaults(ListLength,Start,End,Step),
    S = start_transform(ListLength,Start1),
    E = end_transform(ListLength,End1),
    ?debugFmt("slice_transform: S,C1,E,C2,Step1: ~p,~p,~p,~p,~p~n",[S,C1,E,C2,Step1]),
    slice_list(List,ListLength,S,C1,E,C2,Step1).

%[N:N:N]
slice_list(_List,_ListLength,Start,_C1,End,_C2,Step) when Start > End, Step > 0 ->
    throw(outOfBounds);
slice_list(_List,_ListLength,Start,_C1,End,_C2,Step) when Start < End andalso Step < 0 ->
	  throw(outOfBounds);
slice_list(_List,_ListLength,Start,_C1,End,_C2,_Step) when Start < 0 andalso End < 0 ->
	  throw(outOfBounds);
slice_list(_List,ListLength,Start,_C1,End,_C2,_Step) when Start > ListLength andalso End > ListLength-1 ->
	  throw(outOfBounds);
slice_list(List,ListLength,Start,_C1,End,_C2,Step) when Step > 0 ->
    {LowerBound,UpperBound} = index_bounds(Step,ListLength,Start,End),
    ?debugFmt("LowerBound+1, UpperBound+1, UpperBound - LowerBound + 1: ~p, ~p, ~p~n",[LowerBound+1,UpperBound,UpperBound-LowerBound]),
    BoundList = lists:sublist(List,LowerBound+1,UpperBound-LowerBound),
    SequenceList = lists:seq(1,erlang:length(BoundList),Step),
    lists:map(fun (N) -> lists:nth(N,BoundList) end,SequenceList);
slice_list(List,ListLength,Start,_C1,End,_C2,Step) when Step < 0 ->
    {LowerBound,UpperBound} = index_bounds(Step,ListLength,Start,End),
    %S1 = S0 - 1,
    ?debugFmt("Start,End: ~p, ~p~n",[Start,End]),
    case erlang:abs(End) > ListLength of
        true ->
            ?debugFmt("LowerBound, UpperBound, UpperBound - LowerBound + 1: ~p, ~p, ~p~n",[LowerBound+1,UpperBound,UpperBound-LowerBound+1]),
            BoundList = lists:sublist(List, LowerBound+1, UpperBound - LowerBound + 1);
        false ->
            ?debugFmt("LowerBound+2, UpperBound, UpperBound - LowerBound: ~p, ~p, ~p~n",[LowerBound+2,UpperBound,UpperBound-LowerBound]),
            BoundList = lists:sublist(List, LowerBound+2, UpperBound - LowerBound)
    end,
    ?debugFmt("BoundList: ~p~n",[BoundList]),
    SequenceList = lists:seq(erlang:length(BoundList),1,Step),
    ?debugFmt("SequenceList: ~p~n",[SequenceList]),
    lists:map(fun (N) -> lists:nth(N,BoundList) end,SequenceList).

index_defaults(ListLength, Start, End, Step) ->
    case Start==[] of
      true -> Start1 = 0;
      false -> Start1 = Start
    end,
    case End==[] of
      true -> End1 = ListLength;
      false -> End1 = End
    end,
    case Step==[] of
      true -> Step1 = 1;
      false -> Step1 = Step
    end,
    {Start1, End1, Step1}.

single_index_bounds(S) ->
    if
       S >= 0 -> LowerBound = S;
       S < 0 -> LowerBound = 0
    end,
    LowerBound.

index_bounds(Step1, ListLength, S, E) ->
    AbsListLength = erlang:abs(ListLength),
    case Step1 < 0 of
        true ->
            ?debugMsg("true"),
            if
                S > AbsListLength -> UpperBound = ListLength;
                S =< AbsListLength -> UpperBound = S
            end,
            if
                E >= 0 ->
                    LowerBound = E;
                    %List1 = tl(List);
                E < 0 ->
                    LowerBound = 0
                    %List1 = List
            end;
        false ->
            ?debugMsg("false"),
            if
                S >= 0 -> LowerBound = S;
                S < 0 -> LowerBound = 0
            end,
            if
                E > AbsListLength -> UpperBound = ListLength;
                E =< AbsListLength -> UpperBound = E
            end
    end,
    ?debugFmt("index_bounds: LowerBound,UpperBound: ~p,~p~n",[LowerBound,UpperBound]),
    {LowerBound, UpperBound}.

parse_index(Index) ->
    ParsedIndex = re:replace(Index, "([0-9\-]+)?(:)?([0-9\-]+)?(:)?([0-9\-]+)?", "\\1 ,\\2 ,\\3 ,\\4 ,\\5 ", [{return,list}]),
    ParsedIndex1 = lists:map(fun(X) -> string:strip(X) end, string:tokens(ParsedIndex, ",")),
    [Start, D1, End, D2, Step] = ParsedIndex1,
    Start1 = cast_to_integer(Start),
    End1 = cast_to_integer(End),
    C1 = parse_colon(D1),
    C2 = parse_colon(D2),
    Step1 = cast_to_integer(Step),
    ?debugFmt("Parsed: Start1, End1, C1, C2, Step1: ~p, ~p, ~p, ~p, ~p~n",[Start1, End1, C1, C2, Step1]),
    {Start1, End1, C1, C2, Step1}.

start_transform(_ListLength, []) ->
    ?debugFmt("start_transform: ~p~n", [0]),
    0;
start_transform(ListLength, Start) ->
    case Start >= 0 orelse erlang:abs(Start) > ListLength of
        true ->
            ?debugFmt("start_transform: ~p~n", [Start]),
            Start;
        false ->
            ?debugFmt("start_transform: ~p~n", [ListLength + Start]),
            ListLength + Start
    end.

end_transform(ListLength, []) ->
    ?debugFmt("end_transform: ~p~n", [ListLength]),
    ListLength;
end_transform(ListLength, End) ->
    case End >= 0 orelse erlang:abs(End) > ListLength of
        true ->
            ?debugFmt("end_transform: ~p~n", [End]),
            End;
        false ->
            ?debugFmt("end_transform: ~p~n", [ListLength + End]),
            ListLength + End
    end.

cast_to_integer([]) ->
    [];
cast_to_integer(Input) when is_list(Input)->
        case lists:member($., Input) of
                true ->
                        erlang:round(erlang:list_to_float(Input));
                false ->
                        erlang:list_to_integer(Input)
        end.

parse_colon([]) ->
    false;
parse_colon(Colon) ->
    case Colon of
        ":" -> true;
        _ -> false
    end.
