% A mvar is a process that holds a value (its content) and provides exclusive access to it. When a mvar terminates it executes it given finalize procedure, which is needed for content that needs to clean up when terminating. When a mvar is created it executes its supplied initialize procedure, which creates the initial content from within the mvar process so if the initial content is another linked dependent process (such as a socket) it will terminate when the mvar terminates without the need for a finalizer. A mvar itself dependently links to its parent process (the process that created it) and thus terminates when its parent process terminates.
-module (mvar).

-export_type ([mvar/1]).
-export ([create/2, new/2, new/1]).
-export ([modify/2, with/2, read/1, write/2]).
-export ([terminate/1]).

-behaviour (gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type mvar(_) :: pid() | atom().
% Unregistered or registered process holding a value of given paramterized type

-type initializer(A) :: fun (() -> A). % IO throws X
% Creates initial value. Any throw will be caught and re-thrown in the creating process.
-type finalizer(A) :: fun ((A) -> ok). % IO
% Closes supplied value. Any exception will not be caught causing an exit signal to be sent to parent (creating) process.

-spec create (initializer(A), finalizer(A)) -> mvar(A). % IO throws X
% Create new mvar with initial content created from initializer (run within new mvar process so it owns it). Any throw in initializer will be caught and re-thrown in the calling process. Other exceptions will not be caught causing an exit signal to be sent to calling process. When the mvar terminates then given finalizer will be executed against current content. Any exception raised in finalizer (when terminating) will be sent as an exit signal to the parent (calling) process.
create (Initialize, Finalize) ->
	Ref = make_ref(),
	case gen_server:start_link (?MODULE, {self(), Ref, Initialize, Finalize}, []) of
		{ok, Pid} -> Pid;
		ignore -> receive {mvar_init_throw, Ref, Thrown} -> throw (Thrown) end end.

-spec new (A, finalizer(A)) -> mvar(A). % IO
% Same as `create` except initial value given directly
new (Value, Finalize) -> create (fun () -> Value end, Finalize).

-spec new (A) -> mvar(A). % IO
% Same as `new/2` except no finalizer
new (Value) -> new (Value, fun (_) -> ok end).

-type modifier(A,B) :: fun ((A) -> {A, B}). % IO throws X

-spec modify (mvar(A), modifier(A,B)) -> B. % IO throws X
% Atomically modify content and return associated result. Any throw is caught and re-thrown in caller. Errors are not caught and will terminate var and send exit signal to parent.
modify (Var, Modify) -> case gen_server:call (Var, {modify, Modify}) of
	{ok, B} -> B;
	{throw, Thrown} -> throw (Thrown) end.

-spec with (mvar(A), fun ((A) -> B)) -> B. % IO throws X, fun IO throws X
% Execute Procedure with exclusive access to content but don't modify it.
with (Var, Act) -> modify (Var, fun (A) -> {A, Act (A)} end).

-spec read (mvar(A)) -> A. % IO
% Return content
read (Var) -> with (Var, fun (A) -> A end).

-spec write (mvar(A), A) -> A. % IO
% Change content and return previous content
write (Var, Value) -> modify (Var, fun (A) -> {Value, A} end).

-spec terminate (mvar(_)) -> ok. % IO
% Terminate mvar. Its finalizer will be executed. Future accesses to this mvar will fail.
terminate (Var) -> gen_server:call (Var, stop).

% gen_server callbacks %

-type state(A) :: {A, finalizer(A)}.

-spec init ({pid(), reference(), initializer(A), finalizer(A)}) -> {ok, state(A)} | ignore. % IO
% Create initial value using initializer and return it in state with finalizer. Catch throws in initializer and report it to caller via direct send and `ignore` result. `create` will pick this up an re-throw it in caller. An error in initializer will cause process to abort and exit signal being sent to caller.
init ({Caller, ThrowRef, Initialize, Finalize}) -> try Initialize()
	of A -> {ok, {A, Finalize}}
	catch Thrown -> Caller ! {mvar_init_throw, ThrowRef, Thrown}, ignore end.

-spec handle_call
	({modify, modifier(A,B)}, {pid(), tag()}, state(A)) -> {reply, {ok, B} | {throw, any()}, state(A)};
	(stop, {pid(), tag()}, state(A)) -> {stop, normal, ok, state(A)}. % IO
% Modify content and return associated value. Catch any throws and return them to `modify` to be re-thrown. Errors will abort this mvar process and send exit signal to linked owner.
handle_call ({modify, Modify}, _From, {A, X}) -> try Modify (A)
	of {A1, B} -> {reply, {ok, B}, {A1, X}}
	catch Thrown -> {reply, {throw, Thrown}, {A, X}} end;
% Terminate mvar
handle_call (stop, _From, State) -> {stop, normal, ok, State}.

-spec terminate (reason(), state(_)) -> any(). % IO. Result ignored
% Execute finalizer upon termination
terminate (_Reason, {A, Finalize}) -> Finalize (A).

-type tag() :: any(). % Unique tag
-type reason() :: any().

handle_cast (_Request, State) -> {noreply, State}.
handle_info (_Info, State) -> {noreply, State}.
code_change (_OldVsn, State, _Extra) -> {ok, State}.
