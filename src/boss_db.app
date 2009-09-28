{application, boss_db,
    [{description, "BossDB Database Layer"},
    {vsn, "0.01"},
    {modules, [
        boss_db,
        boss_db_app,
        boss_db_controller,
        boss_db_sup
            ]},
    {registered, []},
    {mod, {boss_db_app, []}},
    {env, []},
    {applications, [kernel, stdlib]}]}.
