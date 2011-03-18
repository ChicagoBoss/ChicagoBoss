% Wire protocol message types (records)

-type db() :: atom().

-type collection() :: atom(). % without db prefix

-type cursorid() :: integer().

-type selector() :: bson:document().

-record (insert, {
	collection :: collection(),
	documents :: [bson:document()] }).

-record (update, {
	collection :: collection(),
	upsert = false :: boolean(),
	multiupdate = false :: boolean(),
	selector :: selector(),
	updater :: bson:document() | modifier() }).

-type modifier() :: bson:document().

-record (delete, {
	collection :: collection(),
	singleremove = false :: boolean(),
	selector :: selector() }).

-record (killcursor, {
	cursorids :: [cursorid()] }).

-record ('query', {
	tailablecursor = false :: boolean(),
	slaveok = false :: boolean(),
	nocursortimeout = false :: boolean(),
	awaitdata = false :: boolean(),
	collection :: collection(),
	skip = 0 :: skip(),
	batchsize = 0 :: batchsize(),
	selector :: selector(),
	projector = [] :: projector() }).

-type projector() :: bson:document().
-type skip() :: integer().
-type batchsize() :: integer(). % 0 = default batch size. negative closes cursor

-record (getmore, {
	collection :: collection(),
	batchsize = 0 :: batchsize(),
	cursorid :: cursorid() }).

-record (reply, {
	cursornotfound :: boolean(),
	queryerror :: boolean(),
	awaitcapable :: boolean(),
	cursorid :: cursorid(),
	startingfrom :: integer(),
	documents :: [bson:document()] }).
