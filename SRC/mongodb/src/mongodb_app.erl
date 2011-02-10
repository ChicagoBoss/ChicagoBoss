%% Init some internal global variables used by mongodb app
-module (mongodb_app).

-behaviour (application).
-export ([start/2, stop/1]).

-behaviour (supervisor).
-export ([init/1]).

-export ([gen_objectid/0, next_requestid/0]). % API

%% Behaviour callbacks

start (_, []) -> supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

stop (_) -> ok.

%% Supervisor callbacks

%% Create global vars which will be owned by this supervisor (and die with it)
init ([]) ->
	ets:new (?MODULE, [named_table, public]),
	ets:insert (?MODULE, [
		{oid_counter, 0},
		{oid_machineprocid, oid_machineprocid()},
		{requestid_counter, 0} ]),
	{ok, {{one_for_one,3,10}, []}}.

%% API functions

-spec next_requestid () -> mongo_protocol:requestid(). % IO
% Fresh request id
next_requestid() -> ets:update_counter (?MODULE, requestid_counter, 1).

-spec gen_objectid () -> bson:objectid(). % IO
% Fresh object id
gen_objectid() ->
	Now = bson:unixtime_to_secs (bson:timenow()),
	MPid = ets:lookup_element (?MODULE, oid_machineprocid, 2),
	N = ets:update_counter (?MODULE, oid_counter, 1),
	bson:objectid (Now, MPid, N).

-spec oid_machineprocid () -> <<_:40>>. % IO
% Fetch hostname and os pid and compress into a 5 byte id
oid_machineprocid() ->
	OSPid = list_to_integer (os:getpid()),
	{ok, Hostname} = inet:gethostname(),
	<<MachineId:3/binary, _/binary>> = erlang:md5 (Hostname),
	<<MachineId:3/binary, OSPid:16/big>>.
