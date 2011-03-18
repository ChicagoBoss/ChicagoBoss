%%%-------------------------------------------------------------------
%%% File    : medici.hrl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 Sept 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------

%% General defaults

-define(CONTROLLER_NAME, medici).
%% If you change NUM_CLIENTS you should probably also change the 
%% thnum parameter when starting the tyrant server.
-define(NUM_CLIENTS, 8). 
-define(AUTO_SYNC, false).
-define(AUTO_TUNE, false).
-define(DEFAULT_SYNC_PERIOD, 5000).
-define(DEFAULT_TUNE_PERIOD, 300000).
-define(DEFAULT_COPY_PERIOD, 300000).


%% Defaults for the tyrant port server

-define(PORT_SRV_NAME, medici_srv).
%% XXX Jim: Either the in or out option should be added here as well (nothing is being
%%     send to the port, we are just reading its output) depending on how  
-define(PORT_OPTS, [binary, 
		    use_stdio, 
		    stderr_to_stdout,
		    stream, 
		    {line, 256}, 
		    exit_status, 
		    hide]).
%% The next line is almost certainly wrong unless you are using a Mac and MacPorts...
-define(TYRANT_BIN, "/opt/local/bin/ttserver").
-define(TYRANT_OPTS, []).
-define(DATA_FILE, "\"*\""). % default to in-memory hash (quote the *...)
-define(TUNING_OPTS, []).
-define(LOG_REGEXP, "\\S+\\t(\\S+)").
-define(PID_REGEXP, "service started: (\\d+)").

%% Testing and debugging bits

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(DEBUG).
-define(DEBUG_LOG(Msg, Args), error_logger:error_msg(Msg, Args)).
-else.
-define(DEBUG_LOG(_Msg, _Args), void).
-endif.
