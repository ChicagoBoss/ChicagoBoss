-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-record(cookie, { name, value, path="/", minutes_to_live=20 }).
-record(header, { name, value }).
-record(response, { statuscode=200, headers=[], cookies=[], data=[] }).
-record(uploaded_file, { original_name, temp_file, size }).