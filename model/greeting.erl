-module(greeting, [Id, GreetingText]).
-compile(export_all).

validation_tests() ->
    [{fun() -> length(GreetingText) >= 4 end,
            "Greeting must be at least 4 characters long!"
        }].