{application, boss_db_test,
    [{description, "Chicago Boss DB Testing Application"},
    {vsn, "0.1"},
    {modules, [
        boss_db_test,
        boss_db_test_app
        ]},
    {registered, []},
    {mod, {boss_db_test_app, []}},
    {env, []},
    {applications, [kernel, stdlib]}]}.
