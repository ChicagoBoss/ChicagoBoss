{application, boss_session_test,
    [{description, "Chicago Boss Session Testing Application"},
    {vsn, "0.1"},
    {modules, [
        boss_session_test,
        boss_session_test_app
        ]},
    {registered, []},
    {mod, {boss_session_test_app, []}},
    {env, []},
    {applications, [kernel, stdlib]}]}.
