%%---------------------------------------------------------------------
%% BIG COPYRIGHT NOTE:
%%
%% This file comes directly from the ErlyDTL project:
%%
%%     http://code.google.com/p/erlydtl/
%%
%% I made some superficial changes, and also split the format function 
%% to be able to format times separate from datetimes (which is needed 
%% for the 'time' filter). All the real work in this file - all the good 
%% stuff - was carried out by Evan and Roberto. 
%%
%% The credit is entirely theirs! 
%%---------------------------------------------------------------------

%%% File:      erlydtl_dateformat.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc
%%% Time/Date formatting for ErlyDTL
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2008 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(etcher_dateformat).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-export([format_date/1,
         format_date/2,
         format_time/1,
         format_time/2
         ]).

-define(IS_TIME_TAG(C),
    C =:= $a orelse
    C =:= $A orelse
    C =:= $B orelse
    C =:= $f orelse
    C =:= $g orelse
    C =:= $G orelse
    C =:= $h orelse
    C =:= $H orelse
    C =:= $i orelse
    C =:= $P orelse
    C =:= $s 
).

-define(IS_DATE_TAG(C),
    C =:= $b orelse
    C =:= $d orelse
    C =:= $D orelse
    C =:= $F orelse
    C =:= $I orelse
    C =:= $j orelse
    C =:= $l orelse
    C =:= $L orelse
    C =:= $m orelse
    C =:= $M orelse
    C =:= $n orelse
    C =:= $N orelse
    C =:= $O orelse
    C =:= $r orelse
    C =:= $S orelse
    C =:= $t orelse
    C =:= $T orelse
    C =:= $U orelse
    C =:= $w orelse
    C =:= $W orelse
    C =:= $y orelse
    C =:= $Y orelse
    C =:= $z orelse
    C =:= $Z
).

%
% Format the current date/time
%
format_date(FormatString) ->
    format_date(erlang:localtime(), FormatString).

%
% Format a tuple of the form {Y,M,D}
%
format_date({_,_,_} = Date, FormatString) ->
    format_date({Date, {0,0,0}}, FormatString);
%
% Format a tuple of the form {{Y,M,D},{H,M,S}}
% This is the format returned by erlang:localtime()
% and other standard date/time BIFs
%
format_date({{_,_,_} = Date,{_,_,_} = Time}, FormatString) ->
    do_date_format(FormatString, Date, Time).

%
% Format the current time
%
format_time(FormatString) ->
    {_Date, Time} = erlang:localtime(),
    format_time(Time, FormatString).

format_time({_,_,_} = Time, FormatString) ->
    do_time_format(FormatString, Time).

do_date_format(FormatString, Date, Time) ->
    do_date_format(FormatString, Date, Time, []).

do_date_format([C | Rest], Date, Time, Acc) when ?IS_DATE_TAG(C) ->
    S = date_tag_to_value(C, Date, Time),
    do_date_format(Rest, Date, Time, [S | Acc]);
do_date_format([C | Rest], Date, Time, Acc) when ?IS_TIME_TAG(C) ->
    S = time_tag_to_value(C, Time),
    do_date_format(Rest, Date, Time, [S | Acc]);
do_date_format([$\\, C | Rest], Date, Time, Acc) ->
    do_date_format(Rest, Date, Time, [C | Acc]);
do_date_format([C | Rest], Date, Time, Acc) ->
    do_date_format(Rest, Date, Time, [C | Acc]);
do_date_format([], _Date, _Time, Acc) ->
    unicode:characters_to_list(lists:reverse(Acc)).

do_time_format(FormatString, Time) ->
    do_time_format(FormatString, Time, []).

do_time_format([C | _], _Time, _Acc) when ?IS_DATE_TAG(C) ->
    % Date tags should have been: (a) excluded; or (b) escaped
    "";
do_time_format([C | Rest], Time, Acc) when ?IS_TIME_TAG(C) ->
    S = time_tag_to_value(C, Time),
    do_time_format(Rest, Time, [S | Acc]);
do_time_format([$\\, C | Rest], Time, Acc) ->
    do_time_format(Rest, Time, [C | Acc]);
do_time_format([C | Rest], Time, Acc) ->
    do_time_format(Rest, Time, [C | Acc]);
do_time_format([], _Time, Acc) ->
    unicode:characters_to_list(lists:reverse(Acc)).

%-----------------------------------------------------------
% Time formatting
%-----------------------------------------------------------

% 'a.m.' or 'p.m.'
time_tag_to_value($a, {H, _, _}) when H > 11 -> "p.m.";
time_tag_to_value($a, _) -> "a.m.";

% 'AM' or 'PM'
time_tag_to_value($A, {H, _, _}) when H > 11 -> "PM";
time_tag_to_value($A, _) -> "AM";

% Swatch Internet time
time_tag_to_value($B, _) ->
   ""; % NotImplementedError

%
% Time, in 12-hour hours and minutes, with minutes
% left off if they're zero.
%
% Examples: '1', '1:30', '2:05', '2'
%
% Proprietary extension.
%
time_tag_to_value($f, {H, 0, S}) ->
   % If min is zero then return the hour only
   time_tag_to_value($g, {H, 0, S});
time_tag_to_value($f, Time) ->
   % Otherwise return hours and mins
   time_tag_to_value($g, Time)
      ++ ":" ++ time_tag_to_value($i, Time);

% Hour, 12-hour format without leading zeros; i.e. '1' to '12'
time_tag_to_value($g, {H,_,_}) ->
   integer_to_list(hour_24to12(H));

% Hour, 24-hour format without leading zeros; i.e. '0' to '23'
time_tag_to_value($G, {H,_,_}) ->
   integer_to_list(H);

% Hour, 12-hour format; i.e. '01' to '12'
time_tag_to_value($h, {H,_,_}) ->
   integer_to_list_zerofill(integer_to_list(hour_24to12(H)));

% Hour, 24-hour format; i.e. '00' to '23'
time_tag_to_value($H, {H,_,_}) ->
   integer_to_list_zerofill(H);

% Minutes; i.e. '00' to '59'
time_tag_to_value($i, {_,M,_}) ->
   integer_to_list_zerofill(M);

% Time, in 12-hour hours, minutes and 'a.m.'/'p.m.', with minutes left off
% if they're zero and the strings 'midnight' and 'noon' if appropriate.
% Examples: '1 a.m.', '1:30 p.m.', 'midnight', 'noon', '12:30 p.m.'
% Proprietary extension.
time_tag_to_value($P, {0,  0, _}) -> "midnight";
time_tag_to_value($P, {12, 0, _}) -> "noon";
time_tag_to_value($P, Time) ->
   time_tag_to_value($f, Time)
      ++ " " ++ time_tag_to_value($a, Time);

% Seconds; i.e. '00' to '59'
time_tag_to_value($s, {_,_,S}) ->
   integer_to_list_zerofill(S);

time_tag_to_value(C, Time) ->
    io:format("Unimplemented time tag : ~p [Time : ~p]", [C, Time]),
    "".

%-----------------------------------------------------------
% Date formatting
%-----------------------------------------------------------

% Month, textual, 3 letters, lowercase; e.g. 'jan'
date_tag_to_value($b, {_,M,_}, _) ->
   string:sub_string(monthname(M), 1, 3);

% Day of the month, 2 digits with leading zeros; i.e. '01' to '31'
date_tag_to_value($d, {_, _, D}, _) ->
   integer_to_list_zerofill(D);

% Day of the week, textual, 3 letters; e.g. 'Fri'
date_tag_to_value($D, Date, _) ->
   Dow = calendar:day_of_the_week(Date),
   ucfirst(string:sub_string(dayname(Dow), 1, 3));

% Month, textual, long; e.g. 'January'
date_tag_to_value($F, {_,M,_}, _) ->
   ucfirst(monthname(M));

% '1' if Daylight Savings Time, '0' otherwise.
date_tag_to_value($I, _, _) ->
   ""; % NotImplementedError

% Day of the month without leading zeros; i.e. '1' to '31'
date_tag_to_value($j, {_, _, D}, _) ->
   integer_to_list(D);

% Day of the week, textual, long; e.g. 'Friday'
date_tag_to_value($l, Date, _) ->
   ucfirst(dayname(calendar:day_of_the_week(Date)));

% Boolean for whether it is a leap year; i.e. True or False
date_tag_to_value($L, {Y,_,_}, _) ->
   case calendar:is_leap_year(Y) of
   true -> "True";
   _ -> "False"
   end;

% Month; i.e. '01' to '12'
date_tag_to_value($m, {_, M, _}, _) ->
   integer_to_list_zerofill(M);

% Month, textual, 3 letters; e.g. 'Jan'
date_tag_to_value($M, {_,M,_}, _) ->
   ucfirst(string:sub_string(monthname(M), 1, 3));

% Month without leading zeros; i.e. '1' to '12'
date_tag_to_value($n, {_, M, _}, _) ->
   integer_to_list(M);

% Month abbreviation in Associated Press style. Proprietary extension.
date_tag_to_value($N, {_,M,_}, _) when M =:= 9 ->
   % Special case - "Sept."
   ucfirst(string:sub_string(monthname(M), 1, 4)) ++ ".";
date_tag_to_value($N, {_,M,_}, _) when M < 3 orelse M > 7 ->
   % Jan, Feb, Aug, Oct, Nov, Dec are all
   % abbreviated with a full-stop appended.
   ucfirst(string:sub_string(monthname(M), 1, 3)) ++ ".";
date_tag_to_value($N, {_,M,_}, _) ->
   % The rest are the fullname.
   ucfirst(monthname(M));

% Difference to Greenwich time in hours; e.g. '+0200'
date_tag_to_value($O, Date, Time) ->
   DiffSecs = utc_diff_secs(Date, Time),
   Diff = trunc((DiffSecs / 3600) * 100),
   Offset = if
      Diff < 0 ->
          io_lib:format("-~4..0w", [abs(Diff)]);
      true ->
          io_lib:format("+~4..0w", [Diff])
   end,
   lists:flatten(Offset);

% RFC 2822 formatted date; e.g. 'Thu, 21 Dec 2000 16:01:07 +0200'
date_tag_to_value($r, Date, Time) ->
   do_date_format("D, j M Y H:i:s O", Date, Time);

% English ordinal suffix for the day of the month, 2 characters;
% i.e. 'st', 'nd', 'rd' or 'th'
date_tag_to_value($S, {_, _, D}, _) when
   D rem 100 =:= 11 orelse
   D rem 100 =:= 12 orelse
   D rem 100 =:= 13 -> "th";
date_tag_to_value($S, {_, _, D}, _) when D rem 10 =:= 1 -> "st";
date_tag_to_value($S, {_, _, D}, _) when D rem 10 =:= 2 -> "nd";
date_tag_to_value($S, {_, _, D}, _) when D rem 10 =:= 3 -> "rd";
date_tag_to_value($S, _, _) -> "th";

% Number of days in the given month; i.e. '28' to '31'
date_tag_to_value($t, {Y,M,_}, _) ->
   integer_to_list(calendar:last_day_of_the_month(Y,M));

% Time zone of this machine; e.g. 'EST' or 'MDT'
date_tag_to_value($T, _, _) ->
    etcher_tz:timezone_abbr();

% Seconds since the Unix epoch (January 1 1970 00:00:00 GMT)
date_tag_to_value($U, Date, Time) ->
    EpochSecs = calendar:datetime_to_gregorian_seconds({Date, Time})
       - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    integer_to_list(EpochSecs);

% Day of the week, numeric, i.e. '0' (Sunday) to '6' (Saturday)
date_tag_to_value($w, Date, _) ->
   % Note: calendar:day_of_the_week returns
   %   1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7
   integer_to_list(calendar:day_of_the_week(Date) rem 7);

% ISO-8601 week number of year, weeks starting on Monday
date_tag_to_value($W, {Y,M,D}, _) ->
   integer_to_list(year_weeknum(Y,M,D));

% Year, 2 digits; e.g. '99'
date_tag_to_value($y, {Y, _, _}, _) ->
   string:sub_string(integer_to_list(Y), 3);

% Year, 4 digits; e.g. '1999'
date_tag_to_value($Y, {Y, _, _}, _) ->
   integer_to_list(Y);

% Day of the year; i.e. '0' to '365'
date_tag_to_value($z, {Y,M,D}, _) ->
    integer_to_list(day_of_year(Y,M,D));

% Time zone offset in seconds (i.e. '-43200' to '43200'). The offset for
% timezones west of UTC is always negative, and for those east of UTC is
% always positive.
date_tag_to_value($Z, _, _) ->
   {NowDate, NowTime} = erlang:localtime(),
   DiffSecs = utc_diff_secs(NowDate, NowTime),
   integer_to_list(DiffSecs);

date_tag_to_value(C, Date, Time) ->
    io:format("Unimplemented date tag : ~p [Date : ~p] [Time : ~p]",
        [C, Date, Time]),
    "".

% Date helper functions
day_of_year(Y,M,D) ->
   day_of_year(Y,M,D,0).
day_of_year(_Y,M,D,Count) when M =< 1 ->
   D + Count;
day_of_year(Y,M,D,Count) when M =< 12 ->
   day_of_year(Y, M - 1, D, Count + calendar:last_day_of_the_month(Y,M));
day_of_year(Y,_M,D,_Count) ->
   day_of_year(Y, 12, D, 0).

hour_24to12(0) -> 12;
hour_24to12(H) when H < 13 -> H;
hour_24to12(H) when H < 24 -> H - 12;
hour_24to12(H) -> H.

year_weeknum(Y,M,D) -> 
    First = (calendar:day_of_the_week(Y, 1, 1) rem 7) - 1,
    Wk = ((((calendar:date_to_gregorian_days(Y, M, D) -
            calendar:date_to_gregorian_days(Y, 1, 1)) + First) div 7)
           + (case First < 4 of true -> 1; _ -> 0 end)),
    case Wk of
       0 -> weeks_in_year(Y - 1);
       _ -> case weeks_in_year(Y) of
              WksInThisYear when Wk > WksInThisYear -> 1;
              _ -> Wk
            end
    end.
   
weeks_in_year(Y) ->
    D1 = calendar:day_of_the_week(Y, 1, 1),
    D2 = calendar:day_of_the_week(Y, 12, 31),
    if (D1 =:= 4 orelse D2 =:= 4) -> 53; true -> 52 end.

utc_diff_secs(Date, Time) ->
   LTime = {Date, Time},
   UTime = erlang:localtime_to_universaltime(LTime),
   calendar:datetime_to_gregorian_seconds(LTime) - 
       calendar:datetime_to_gregorian_seconds(UTime).

dayname(1) -> "monday";
dayname(2) -> "tuesday";
dayname(3) -> "wednesday";
dayname(4) -> "thursday";
dayname(5) -> "friday";
dayname(6) -> "saturday";
dayname(7) -> "sunday";
dayname(_) -> "???".

monthname(1) ->  "january";
monthname(2) ->  "february";
monthname(3) ->  "march";
monthname(4) ->  "april";
monthname(5) ->  "may";
monthname(6) ->  "june";
monthname(7) ->  "july";
monthname(8) ->  "august";
monthname(9) ->  "september";
monthname(10) -> "october";
monthname(11) -> "november";
monthname(12) -> "december";
monthname(_) -> "???".

% Utility functions
integer_to_list_zerofill(N) when N < 10 ->
    lists:flatten(io_lib:format("~2..0B", [N]));
integer_to_list_zerofill(N) ->
    integer_to_list(N).

ucfirst([First | Rest]) when First >= $a, First =< $z ->
    [First-($a-$A) | Rest];
ucfirst(Other) ->
    Other.


