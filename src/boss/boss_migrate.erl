%% boss_migrate handles the chicago boss specific bits of migrations,
%% in collaboration with boss_db.

-module(boss_migrate).

-export([make/2,
	 run/1,
	 redo/2]).

%% Returns a sorted list of all files in priv/migrations/.
migration_list(App) ->
    lists:sort(filelib:wildcard(filename:join([boss_files:root_priv_dir(App), "migrations", "*.erl"]))).

%% Create a migration.  MigrationName is an atom to use as the name of
%% the migration.
make(App, MigrationName) when is_atom(MigrationName) ->
    {MegaSeconds, Seconds, _Microsecs} = erlang:now(),
    Filename = filename:join([boss_files:root_priv_dir(App), "migrations",
			      io_lib:format("~p~p_~s.erl", [MegaSeconds, Seconds, MigrationName])]),
    file:write_file(Filename, io_lib:format("%% Migration: ~p~n~n{~p,~n  fun(up) -> undefined;~n     (down) -> undefined~n  end}.~n", [MigrationName, MigrationName])).

%% Run the migrations.
run(App) ->
    boss_db:migrate(load_migrations(App)).

						% Redo {down, up} a specific migration.
redo(App, Tag) ->
    Fun =  proplists:get_value(Tag, load_migrations(App)),
    boss_db:transaction(fun () ->
				boss_db:migrate({Tag, Fun}, down),
				boss_db:migrate({Tag, Fun}, up)
			end).

load_migrations(App) ->
    lists:map(fun(File) ->
		      lager:info("Reading migration file: ~p~n", [File]),
		      {ok, Terms} = file:script(File),
		      Terms
	      end, migration_list(App)).

