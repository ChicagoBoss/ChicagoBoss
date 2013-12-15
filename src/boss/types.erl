-module(types).

-type execution_mode()		:: 'development' | 'production'.
-type application()  	        :: any().
-type language()		:: any().
-type webserver()               :: 'cowboy' | 'mochiweb_http'.
-type cb_node()                 :: node().
-type controller()              :: any().

-export_type([execution_mode/0, application/0, language/0, webserver/0, cb_node/0]).
-export_type([controller/0]).
