-module(types).

-type execution_mode()		:: 'development' | 'production'.
-type application_name()	:: any().
-type language()		:: any().
-type webserver()               :: 'cowboy' | 'mochiweb_http'.
-type cb_node()                 :: node().
-export_type([execution_mode/0, application_name/0, language/0, webserver/0, cb_node/0]).
