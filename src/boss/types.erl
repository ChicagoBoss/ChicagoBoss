-module(types).

-type execution_mode()		:: 'development' | 'production'.
-type application_name()	:: any().
-type language()		:: any().

-export_type([execution_mode/0, application_name/0, language/0]).
