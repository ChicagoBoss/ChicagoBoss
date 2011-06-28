%% Author: dave
%% Created: Mar 1, 2010
%% Description: TODO: Add description to po_scanner
-module(po_scanner).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([scan/1]).

%%
%% API Functions
%%
scan(Path) -> 
    case file:read_file(Path) of
        {ok,File} ->
			Str = re:replace(File, "\\\\n", "\\\n", [global, {return,list}]),
            scan(Str, [], {1, 1}, [in_text]);
        _Error ->
            io:format("No po file found at path ~p~n",[Path]),
            []
    end.


scan("#" ++ T, Scanned, {Row, Column}, Status = [in_text]) -> 
    scan(T, Scanned, {Row, Column + 1}, lists:append([{in_comment, []}],Status));
scan("\n" ++ T, Scanned, {Row, _Column}, [{in_comment, Comment}|Status]) -> 
    scan(T, lists:append(Scanned, [{comment, Comment}]), {Row +1 , 1}, Status);
scan([Head | T], Scanned, {Row, Column}, _Status = [{in_comment, Comment}|Stack]) ->
    NewStatus = lists:append([{in_comment, lists:append(Comment,[Head])}],Stack),
    scan(T, Scanned, {Row, Column + 1}, NewStatus);

%%Msg id
scan("msgid" ++ T, Scanned, {Row, Column}, Status = [in_text]) ->  
    scan(T, Scanned, {Row, Column + 5}, lists:append([{in_message_id, []}],Status));

%%scan("msgid" ++ T, Scanned, {Row, Column}, [{in_message_str, Body}|Stack]) ->  
%%	scan(T, lists:append(Scanned , [{str, Body}]), {Row, Column + 5}, lists:append([{in_message_id, []}],Stack));

scan("\n\n" ++ T, Scanned, {Row, _Column}, [{in_message_str, Body}|Stack]) ->  
    scan(T, lists:append(Scanned , [{str, Body}]), {Row + 2, 1}, Stack);
scan("\n", Scanned, {Row, _Column}, [{in_message_str, Body}|Stack]) ->
    scan([], lists:append(Scanned , [{str, Body}]), {Row + 2, 1}, Stack);

%%Msg str
scan("msgstr" ++ T, Scanned, {Row, Column}, [{in_message_id, Body} | Stack]) ->
    %%io:format("Id is ~s~n",[Body]),
    scan(T, lists:append(Scanned ,[{id, Body}]), {Row, Column + 6}, lists:append([{in_message_str, []}],Stack));


scan([$\\, C|T], Scanned, {Row, Column}, [{in_string_body, Body}|Stack]) ->
    scan(T, Scanned, {Row, Column + 2}, [{in_string_body, lists:append(Body, [C])} | Stack]);

%%Start and end for a message body
scan("\"" ++ T, Scanned, {Row, Column}, [{in_string_body, Body}|Stack]) ->
    %%io:format("Ending string ~s ~p~n",[Body, Stack]),
    end_of_string(Body, Stack, T, Scanned, Row, Column);
scan("\"" ++ T, Scanned, {Row, Column}, Stack) ->
  scan(T, Scanned, {Row, Column + 1}, lists:append([{in_string_body, []}], Stack));

%%Carriage return are ignored
%% scan("\n" ++ T, Scanned, {Row, _Column}, Status) ->
%%     scan(T, Scanned, {Row + 1, 1}, Status);

%%Concat string body to already parsed
scan([H | T] , Scanned, {Row, Column}, [{in_string_body, Body} | Stack]) ->
    scan(T, Scanned, {Row, Column + 1}, [{in_string_body, lists:append(Body, [H])} | Stack]);

%%Others characters are ignored
scan([_H | T] , Scanned, {Row, Column}, Status) ->
    scan(T, Scanned, {Row, Column + 1}, Status);

%%EOF
scan([], Scanned, {_Row, _Column}, _Stack) ->Scanned;
scan(In, Scanned, {_Row, _Column}, _Status) ->
    io:format("Cannot process ~p, scanned ~p ~n",[In, Scanned]).

end_of_string(String, [{in_message_id, Body}|Stack] ,T, Scanned, Row, Column) ->
    scan(T, Scanned, {Row, Column}, [{in_message_id, lists:append(Body ,String)} | Stack ]);
end_of_string(String, [{in_message_str, Body}|Stack] , T, Scanned, Row, Column) ->
    scan(T, Scanned, {Row, Column }, [{in_message_str, lists:append(Body,String)} |Stack ]).
	
	

%%
%% Local Functions
%%

