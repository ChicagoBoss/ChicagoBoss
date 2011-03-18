{application, epgsql,
 [{description, "PostgreSQL Client"},
  {vsn, "1.2"},
  {modules, [pgsql, pgsql_binary, pgsql_connection, pgsql_fdatetime,
             pgsql_idatetime, pgsql_sock, pgsql_types]},
  {registered, []},
  {applications, [kernel, stdlib, crypto, ssl]},
  {included_applications, []}]}.
