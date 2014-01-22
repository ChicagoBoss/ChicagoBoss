Testing with EUnit and Proper

I have been adding tests to ChicagoBoss and BossDB via Eunit and
Proper. There are generally 2 types of tests unit tests with EUnit and
property based tests with Proper.

Propety based tests randomly generate a large number of random test
cases and apply the code then validate the responses against rules. In
some cases these tests can be derived directly from the function's
-spec declaration, in others you may need to write an explicit
property to test the issue.

If you are writting properties you can run them by including the file
"std.hrl" in your test file then creating a function like this, Where
each element in the list is a function or a tuple showing the name and
arity of a function.

----
spec_test_() ->
    gen([
         fun prop_pull_recieve/0,
         fun prop_pull_recieve_timeout/0,
         fun prop_pull_recieve_error/0,
         {stop, 0},
         {convert_to_ms, 1},
         {make_queue_options, 0}
        ], boss_mq).

----

By default it will run the tests in parallel and run 100 instances of
each test. You can adjust those if you need to. 
