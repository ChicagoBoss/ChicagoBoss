-module(types).

-type execution_mode()		:: 'development' | 'production'.
-type application()  	        :: atom().
-type language()		:: any().
-type webserver()               :: 'cowboy' | 'mochiweb_http'.
-type cb_node()                 :: node().
-type controller()              :: any().

-type compiler_adapters()       :: 'boss_compiler_adapter_elixir' | 
                                   'boss_compiler_adapter_erlang' | 
                                   'boss_compiler_adapter_lfe'.

-type template_adapters()       :: 'boss_template_adapter_erlydtl' |
                                   'boss_template_adapter_jade' |
                                   'boss_template_adapter_eex'.

-export_type([execution_mode/0, application/0, language/0, webserver/0, cb_node/0]).
-export_type([controller/0, compiler_adapters/0]).
-export_type([template_adapters/0]).
