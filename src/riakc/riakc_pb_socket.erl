%% -------------------------------------------------------------------
%%
%% riakc_pb_socket: protocol buffer client
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(riakc_pb_socket).
-include_lib("kernel/include/inet.hrl").
-include("riakclient_pb.hrl").
-include("riakc_pb.hrl").
-behaviour(gen_server).

-export([start_link/2, start_link/3,
         start/2, start/3,
         stop/1,
         set_options/2, set_options/3,
         is_connected/1, is_connected/2,
         ping/1, ping/2,
         get_client_id/1, get_client_id/2,
         set_client_id/2, set_client_id/3,
         get_server_info/1, get_server_info/2,
         get/3, get/4, get/5,
         put/2, put/3, put/4,
         delete/3, delete/4, delete/5,
         delete_vclock/4, delete_vclock/5, delete_vclock/6,
         delete_obj/2, delete_obj/3, delete_obj/4,
         list_buckets/1, list_buckets/2, list_buckets/3,
         list_keys/2, list_keys/3,
         stream_list_keys/2, stream_list_keys/3, stream_list_keys/4,
         get_bucket/2, get_bucket/3, get_bucket/4,
         set_bucket/3, set_bucket/4, set_bucket/5,
         mapred/3, mapred/4, mapred/5,
         mapred_stream/4, mapred_stream/5, mapred_stream/6,
         mapred_bucket/3, mapred_bucket/4, mapred_bucket/5,
         mapred_bucket_stream/5, mapred_bucket_stream/6,
         search/3, search/5, search/6,
         default_timeout/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PROTO_MAJOR, 1).
-define(PROTO_MINOR, 0).
-define(DEFAULT_TIMEOUT, 60000).
-define(FIRST_RECONNECT_INTERVAL, 100).
-define(MAX_RECONNECT_INTERVAL, 30000).

-type address() :: string() | atom() | inet:ip_address().
-type portnum() :: non_neg_integer().
-type option()  :: queue_if_disconnected | {queue_if_disconnected, boolean()} |
                   auto_reconnect | {auto_reconnect, boolean()}.
-type options() :: [option()].
-type client_id() :: binary().
-type bucket() :: binary().
-type key() :: binary().
-type riakc_obj() :: tuple().
-type riak_pbc_options() :: list().
-type req_id() :: non_neg_integer().
-type rpb_req() :: atom() | tuple().
-type ctx() :: any().
-type rpb_resp() :: atom() | tuple().
-type server_prop() :: {node, binary()} | {server_version, binary()}.
-type server_info() :: [server_prop()].
-type bucket_prop() :: {n_val, pos_integer()} | {allow_mult, boolean()}.
-type bucket_props() :: [bucket_prop()].

-record(state, {address,    % address to connect to
                port,       % port to connect to
                auto_reconnect = false, % if true, automatically reconnects to server
                                        % if false, exits on connection failure/request timeout
                queue_if_disconnected = false, % if true, add requests to queue if disconnected
                sock,       % gen_tcp socket
                active,     % active request
                queue,      % queue of pending requests
                connects=0, % number of successful connects
                failed=[],  % breakdown of failed connects
                connect_timeout=infinity, % timeout of TCP connection
                reconnect_interval=?FIRST_RECONNECT_INTERVAL}).
-record(request, {ref :: reference(), msg :: rpb_req(), from, ctx :: ctx(), timeout :: integer(),
                  tref :: reference() | undefined }).


%% @doc Create a linked process to talk with the riak server on Address:Port
%%      Client id will be assigned by the server.
-spec start_link(address(), portnum()) -> {ok, pid()} | {error, term()}.
start_link(Address, Port) ->
    start_link(Address, Port, []).

%% @doc Create a linked process to talk with the riak server on Address:Port with Options
%%      Client id will be assigned by the server.
%%      Options:
%%        auto_reconnect automatically try and reconnenct the socket on dropped socket
%%          or timeout.
%%        queue_if_disconnected adds requests to the queue when it is disconnected
%%          rather than return {error, disconnected}.  If the server comes back the
%%          request will be submitted if the timeout has not expired.  Implies
%%          {auto_reconnect, true}.
%%      
-spec start_link(address(), portnum(), options()) -> {ok, pid()} | {error, term()}.
start_link(Address, Port, Options) when is_list(Options) ->
    gen_server:start_link(?MODULE, [Address, Port, Options], []).

%% @doc Create a process to talk with the riak server on Address:Port
%%      Client id will be assigned by the server.
-spec start(address(), portnum()) -> {ok, pid()} | {error, term()}.
start(Address, Port) ->
    start(Address, Port, []).

%% @doc Create a process to talk with the riak server on Address:Port with Options
%%     See start_link/3.
-spec start(address(), portnum(), options()) -> {ok, pid()} | {error, term()}.
start(Address, Port, Options) when is_list(Options) ->
    gen_server:start(?MODULE, [Address, Port, Options], []).

%% @doc Disconnect the socket and stop the process
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @doc Change the options for this socket.  Allows you to connect with one 
%%      set of optionsthen run with another (e.g. connect with no options to
%%      make sure the server is there, then enable queue_if_disconnected)
set_options(Pid, Options) ->
    set_options(Pid, Options, infinity).
%% @doc set_options/2 with a gen_server timeout
set_options(Pid, Options, Timeout) ->
    gen_server:call(Pid, {set_options, Options}, Timeout).

%% @doc Return true if connected to the remote server and {false, [{Reason,integer()}]}
%%      with a list of failed connect reasons
is_connected(Pid) ->
    is_connected(Pid, infinity).

%% @doc See is_connected/1 with gen_server timeout
is_connected(Pid, Timeout) ->
    gen_server:call(Pid, is_connected, Timeout).

%% @doc Ping the server
-spec ping(pid()) -> ok | {error, term()}.
ping(Pid) ->
    ping(Pid, default_timeout(ping_timeout)).

%% @doc Ping the server specifying timeout
-spec ping(pid(), timeout()) -> ok | {error, term()}.
ping(Pid, Timeout) ->
    gen_server:call(Pid, {req, rpbpingreq, Timeout}, infinity).

%% @doc Get the client id for this connection
-spec get_client_id(pid()) -> {ok, client_id()} | {error, term()}.
get_client_id(Pid) ->
    get_client_id(Pid, default_timeout(get_client_id_timeout)).

%% @doc Get the client id for this connection specifying timeout
-spec get_client_id(pid(), timeout()) -> {ok, client_id()} | {error, term()}.
get_client_id(Pid, Timeout) ->
    gen_server:call(Pid, {req, rpbgetclientidreq, Timeout}, infinity).

%% @doc Set the client id for this connection
-spec set_client_id(pid(), client_id()) -> {ok, client_id()} | {error, term()}.
set_client_id(Pid, ClientId) ->
    set_client_id(Pid, ClientId, default_timeout(set_client_id_timeout)).

%% @doc Set the client id for this connection specifying timeout
-spec set_client_id(pid(), client_id(), timeout()) -> {ok, client_id()} | {error, term()}.
set_client_id(Pid, ClientId, Timeout) ->
    gen_server:call(Pid, {req, #rpbsetclientidreq{client_id = ClientId}, Timeout}, infinity).

%% @doc Get the server information for this connection
-spec get_server_info(pid()) -> {ok, server_info()} | {error, term()}.
get_server_info(Pid) ->
    get_server_info(Pid, default_timeout(get_server_info_timeout)).

%% @doc Get the server information for this connection specifying timeout
-spec get_server_info(pid(), timeout()) -> {ok, server_info()} | {error, term()}.
get_server_info(Pid, Timeout) ->
    gen_server:call(Pid, {req, rpbgetserverinforeq, Timeout}, infinity).

%% @doc Get bucket/key from the server
%%      Will return {error, notfound} if the key is not on the server
-spec get(pid(), bucket() | string(), key() | string()) -> {ok, riakc_obj()} | {error, term()}.
get(Pid, Bucket, Key) ->
    get(Pid, Bucket, Key, [], default_timeout(get_timeout)).

%% @doc Get bucket/key from the server specifying timeout
%%      Will return {error, notfound} if the key is not on the server
-spec get(pid(), bucket() | string(), key() | string(),
          timeout() |  riak_pbc_options()) ->
                 {ok, riakc_obj()} | {error, term() | unchanged}.
get(Pid, Bucket, Key, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    get(Pid, Bucket, Key, [], Timeout);

%% @doc Get bucket/key from the server supplying options
%%      [{r, 1}] would set r=1 for the request
%%      [{if_modified, VClock}] will return unchanged if the object's vclock matches
%%      [head] only return the object's metadata, the value is set to <<>>
%%      [deletedvclock] return a vclock if a tombstone is encountered
get(Pid, Bucket, Key, Options) ->
    get(Pid, Bucket, Key, Options, default_timeout(get_timeout)).
 
%% @doc Get bucket/key from the server supplying options and timeout
%%      [{r, 1}] would set r=1 for the request
%%      [{if_modified, VClock}] will return unchanged if the object's vclock matches
%%      [head] only return the object's metadata, the value is set to <<>>
%%      [deletedvclock] return a vclock if a tombstone is encountered
-spec get(pid(), bucket() | string(), key() | string(),
          riak_pbc_options(), timeout()) ->
                 {ok, riakc_obj()} | {error, term() | unchanged}.
get(Pid, Bucket, Key, Options, Timeout) ->
    Req = get_options(Options, #rpbgetreq{bucket = Bucket, key = Key}),
    gen_server:call(Pid, {req, Req, Timeout}, infinity).

%% @doc Put the metadata/value in the object under bucket/key
-spec put(pid(), riakc_obj()) ->
                 ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Pid, Obj) ->
    put(Pid, Obj, []).

%% @doc Put the metadata/value in the object under bucket/key
-spec put(pid(), riakc_obj(), timeout() | riak_pbc_options()) ->
                 ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Pid, Obj, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    put(Pid, Obj, [], Timeout);

%% @doc Put the metadata/value in the object under bucket/key with options
%%      [{w,2}] sets w=2,
%%      [{dw,1}] set dw=1,
%%      [{pw,1}] set pw=1,
%%      [return_body] returns the updated metadata/value
%%      [return_head] returns the updated metadata with the values set as <<>>
%%      [if_not_modified] the put fails unless riakc_obj and database vclocks match
%%      [if_none_match] the put fails if the key already exist
%%      Put throws siblings if the riakc_obj contains siblings
%%      that have not been resolved by calling select_sibling/2 or 
%%      update_value/2 and update_metadata/2.
put(Pid, Obj, Options) ->
    put(Pid, Obj, Options, default_timeout(put_timeout)).

%% @doc Put the metadata/value in the object under bucket/key with options and timeout
%%      [{w,2}] sets w=2,
%%      [{dw,1}] set dw=1,
%%      [{pw,1}] set pw=1,
%%      [return_body] returns the updated metadata/value
%%      [return_head] returns the updated metadata with the values set as <<>>
%%      [if_not_modified] the put fails unless riakc_obj and database vclocks match
%%      [if_none_match] the put fails if the key already exist
%%      Put throws siblings if the riakc_obj contains siblings
%%      that have not been resolved by calling select_sibling/2 or 
%%      update_value/2 and update_metadata/2.
-spec put(pid(), riakc_obj(), riak_pbc_options(), timeout()) -> 
                 ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Pid, Obj, Options, Timeout) ->
    Content = riakc_pb:pbify_rpbcontent({riakc_obj:get_update_metadata(Obj),
                                         riakc_obj:get_update_value(Obj)}),
    Req = put_options(Options,
                      #rpbputreq{bucket = riakc_obj:bucket(Obj),
                                 key = riakc_obj:key(Obj),
                                 vclock = riakc_obj:vclock(Obj),
                                 content = Content}),
    gen_server:call(Pid, {req, Req, Timeout}, infinity).

%% @doc Delete the key/value
-spec delete(pid(), bucket() | string(), key() | string()) -> ok | {error, term()}.
delete(Pid, Bucket, Key) ->
    delete(Pid, Bucket, Key, []).

%% @doc Delete the key/value specifying timeout
-spec delete(pid(), bucket() | string(), key() | string(), 
             timeout() | riak_pbc_options()) -> ok | {error, term()}.
delete(Pid, Bucket, Key, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    delete(Pid, Bucket, Key, [], Timeout);

%% @doc Delete the key/value with options
%%      [{rw,2}] sets rw=2
delete(Pid, Bucket, Key, Options) ->
    delete(Pid, Bucket, Key, Options, default_timeout(delete_timeout)).

%% @doc Delete the key/value with options and timeout
%%      [{rw,2}] sets rw=2
-spec delete(pid(), bucket() | string(), key() | string(), 
             riak_pbc_options(), timeout()) -> ok | {error, term()}.
delete(Pid, Bucket, Key, Options, Timeout) ->
    Req = delete_options(Options, #rpbdelreq{bucket = Bucket, key = Key}),
    gen_server:call(Pid, {req, Req, Timeout}, infinity).

%% @doc Delete the key/value
-spec delete_vclock(pid(), bucket() | string(), key() | string(), vclock:vclock()) -> ok | {error, term()}.
delete_vclock(Pid, Bucket, Key, VClock) ->
    delete_vclock(Pid, Bucket, Key, VClock, []).

%% @doc Delete the key/value specifying timeout
-spec delete_vclock(pid(), bucket() | string(), key() | string(), vclock:vclock(),
             timeout() | riak_pbc_options()) -> ok | {error, term()}.
delete_vclock(Pid, Bucket, Key, VClock, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    delete_vclock(Pid, Bucket, Key, VClock, [], Timeout);

%% @doc Delete the key/value with options
%%      [{rw,2}] sets rw=2
delete_vclock(Pid, Bucket, Key, VClock, Options) ->
    delete_vclock(Pid, Bucket, Key, VClock, Options, default_timeout(delete_timeout)).

%% @doc Delete the key/value with options and timeout
%%      [{rw,2}] sets rw=2
-spec delete_vclock(pid(), bucket() | string(), key() | string(), vclock:vclock(),
             riak_pbc_options(), timeout()) -> ok | {error, term()}.
delete_vclock(Pid, Bucket, Key, VClock, Options, Timeout) ->
    Req = delete_options(Options, #rpbdelreq{bucket = Bucket, key = Key,
            vclock=VClock}),
    gen_server:call(Pid, {req, Req, Timeout}, infinity).


%% @doc Delete the riak object 
%%      [{rw,2}] sets rw=2
-spec delete_obj(pid(), riakc_obj()) -> ok | {error, term()}.
delete_obj(Pid, Obj) ->
    delete_vclock(Pid, riakc_obj:bucket(Obj), riakc_obj:key(Obj),
        riakc_obj:vclock(Obj), [], default_timeout(delete_timeout)).

%% @doc Delete the riak object with options
%%      [{rw,2}] sets rw=2
-spec delete_obj(pid(), riakc_obj(), riak_pbc_options()) -> ok | {error, term()}.
delete_obj(Pid, Obj, Options) ->
    delete_vclock(Pid, riakc_obj:bucket(Obj), riakc_obj:key(Obj),
        riakc_obj:vclock(Obj), Options, default_timeout(delete_timeout)).

%% @doc Delete the riak object with options and timeout
%%      [{rw,2}] sets rw=2
-spec delete_obj(pid(), riakc_obj(), riak_pbc_options(), timeout()) -> ok | {error, term()}.
delete_obj(Pid, Obj, Options, Timeout) ->
    delete_vclock(Pid, riakc_obj:bucket(Obj), riakc_obj:key(Obj),
        riakc_obj:vclock(Obj), Options, Timeout).

%% @doc List all buckets on the server
-spec list_buckets(pid()) -> {ok, [bucket()]} | {error, term()}.
list_buckets(Pid) ->
    list_buckets(Pid, default_timeout(list_buckets_timeout)).

%% @doc List all buckets on the server specifying server-side timeout
-spec list_buckets(pid(), timeout()) -> {ok, [bucket()]} | {error, term()}.
list_buckets(Pid, Timeout) ->
    list_buckets(Pid, Timeout, default_timeout(list_buckets_call_timeout)).

%% @doc List all buckets on the server specifying server-side and local
%%      call timeout
-spec list_buckets(pid(), timeout(), timeout()) -> {ok, [bucket()]} | 
                                                   {error, term()}.
list_buckets(Pid, Timeout, CallTimeout) ->
    gen_server:call(Pid, {req, rpblistbucketsreq, Timeout}, CallTimeout).

%% @doc List all keys in a bucket
-spec list_keys(pid(), bucket()) -> {ok, [key()]}.
list_keys(Pid, Bucket) ->
    list_keys(Pid, Bucket, default_timeout(list_keys_timeout)).

%% @doc List all keys in a bucket specifying timeout
-spec list_keys(pid(), bucket(), timeout()) -> {ok, [key()]}.
list_keys(Pid, Bucket, Timeout) ->
    case stream_list_keys(Pid, Bucket, Timeout) of
        {ok, ReqId} ->
            wait_for_listkeys(ReqId, Timeout);
        Error ->
            Error
    end.

%% @doc Stream list of keys in the bucket to the calling process.  The
%%      process receives these messages.
%%        {ReqId, {keys, [key()]}}
%%        {ReqId, done}
-spec stream_list_keys(pid(), bucket()) -> {ok, req_id()} | {error, term()}.
stream_list_keys(Pid, Bucket) ->
    stream_list_keys(Pid, Bucket, default_timeout(stream_list_keys_timeout)).

%% @doc Stream list of keys in the bucket to the calling process specifying server side
%%      timeout.
%%      The process receives these messages.
%%        {ReqId, {keys, [key()]}}
%%        {ReqId, done}
-spec stream_list_keys(pid(), bucket(), timeout()) -> {ok, req_id()} | {error, term()}.
stream_list_keys(Pid, Bucket, Timeout) ->
    stream_list_keys(Pid, Bucket, Timeout, default_timeout(stream_list_keys_call_timeout)).

%% @doc Stream list of keys in the bucket to the calling process specifying server side
%%      timeout and local call timeout.
%%      The process receives these messages.
%%        {ReqId, {keys, [key()]}}
%%        {ReqId, done}
-spec stream_list_keys(pid(), bucket(), timeout(), timeout()) -> {ok, req_id()} | 
                                                                 {error, term()}.
stream_list_keys(Pid, Bucket, Timeout, CallTimeout) ->
    ReqMsg = #rpblistkeysreq{bucket = Bucket},
    ReqId = mk_reqid(),
    gen_server:call(Pid, {req, ReqMsg, Timeout, {ReqId, self()}}, CallTimeout).

%% @doc Get bucket properties
-spec get_bucket(pid(), bucket()) -> {ok, bucket_props()} | {error, term()}.
get_bucket(Pid, Bucket) ->
    get_bucket(Pid, Bucket, default_timeout(get_bucket_timeout)).

%% @doc Get bucket properties specifying a server side timeout
-spec get_bucket(pid(), bucket(), timeout()) -> {ok, bucket_props()} | {error, term()}.
get_bucket(Pid, Bucket, Timeout) ->
    get_bucket(Pid, Bucket, Timeout, default_timeout(get_bucket_call_timeout)).

%% @doc Get bucket properties specifying a server side and local call timeout
-spec get_bucket(pid(), bucket(), timeout(), timeout()) -> {ok, bucket_props()} |
                                                           {error, term()}.
get_bucket(Pid, Bucket, Timeout, CallTimeout) ->
    Req = #rpbgetbucketreq{bucket = Bucket},
    gen_server:call(Pid, {req, Req, Timeout}, CallTimeout).

%% @doc Set bucket properties
-spec set_bucket(pid(), bucket(), bucket_props()) -> ok | {error, term()}.
set_bucket(Pid, Bucket, BucketProps) ->
    set_bucket(Pid, Bucket, BucketProps, default_timeout(set_bucket_timeout)).

%% @doc Set bucket properties specifying a server side timeout
-spec set_bucket(pid(), bucket(), bucket_props(), timeout()) -> ok | {error, term()}.
set_bucket(Pid, Bucket, BucketProps, Timeout) ->
    set_bucket(Pid, Bucket, BucketProps, Timeout,
               default_timeout(set_bucket_call_timeout)).

%% @doc Set bucket properties specifying a server side and local call timeout
-spec set_bucket(pid(), bucket(), bucket_props(), timeout(), timeout()) -> ok | {error, term()}.
set_bucket(Pid, Bucket, BucketProps, Timeout, CallTimeout) ->
    PbProps = riakc_pb:pbify_rpbbucketprops(BucketProps),
    Req = #rpbsetbucketreq{bucket = Bucket, props = PbProps},
    gen_server:call(Pid, {req, Req, Timeout}, CallTimeout).

%% @spec mapred(Pid :: pid(),
%%              Inputs :: list(),
%%              Query :: [riak_kv_mapred_query:mapred_queryterm()]) ->
%%       {ok, riak_kv_mapred_query:mapred_result()} |
%%       {error, {bad_qterm, riak_kv_mapred_query:mapred_queryterm()}} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
%% @equiv mapred(Inputs, Query, default_timeout(mapred))
mapred(Pid, Inputs, Query) ->
    mapred(Pid, Inputs, Query, default_timeout(mapred_timeout)).

%% @spec mapred(Pid :: pid(),
%%              Inputs :: list(),
%%              Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%              TimeoutMillisecs :: integer()  | 'infinity') ->
%%       {ok, riak_kv_mapred_query:mapred_result()} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job across the cluster with a cluster timeout.
%%      See the map/reduce documentation for explanation of behavior.
mapred(Pid, Inputs, Query, Timeout) ->
    mapred(Pid, Inputs, Query, Timeout, default_timeout(mapred_call_timeout)).

%% @spec mapred(Pid :: pid(),
%%              Inputs :: list(),
%%              Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%              TimeoutMillisecs :: integer()  | 'infinity',
%%              CallTimeoutMillisecs :: integer()  | 'infinity') ->
%%       {ok, riak_kv_mapred_query:mapred_result()} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job across the cluster with a cluster and local
%%      call timeout.
%%      See the map/reduce documentation for explanation of behavior.
mapred(Pid, Inputs, Query, Timeout, CallTimeout) ->
    case mapred_stream(Pid, Inputs, Query, self(), Timeout, CallTimeout) of
        {ok, ReqId} ->
            wait_for_mapred(ReqId, Timeout);
        Error ->
            Error
    end.

%% @spec mapred_stream(Pid :: pid(),
%%                     Inputs :: list(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                     ClientPid :: pid()) ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a streaming map/reduce job across the cluster sending results
%%      to ClientPid.
%%      See the map/reduce documentation for explanation of behavior.
mapred_stream(Pid, Inputs, Query, ClientPid) ->
    mapred_stream(Pid, Inputs, Query, ClientPid, default_timeout(mapred_stream_timeout)).

%% @spec mapred_stream(Pid :: pid(),
%%                     Inputs :: list(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                     ClientPid :: pid(),
%%                     TimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a streaming map/reduce job with a timeout across the cluster.
%%      sending results to ClientPid.
%%      See the map/reduce documentation for explanation of behavior.
mapred_stream(Pid, Inputs, Query, ClientPid, Timeout) ->
    mapred_stream(Pid, Inputs, Query, ClientPid, Timeout,
                  default_timeout(mapred_stream_call_timeout)).

%% @spec mapred_stream(Pid :: pid(),
%%                     Inputs :: list(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                     ClientPid :: pid(),
%%                     TimeoutMillisecs :: integer() | 'infinity',
%%                     CallTimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a streaming map/reduce job with a map/red timeout across the cluster,
%%      a local call timeout and sending results to ClientPid.
%%      See the map/reduce documentation for explanation of behavior.
mapred_stream(Pid, Inputs, Query, ClientPid, Timeout, CallTimeout) ->
    MapRed = [{'inputs', Inputs},
              {'query', Query},
              {'timeout', Timeout}],
    send_mapred_req(Pid, MapRed, ClientPid, CallTimeout).

%% @spec mapred_bucket(Pid :: pid(),
%%                     Bucket :: bucket(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()]) ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job against a bucket across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
mapred_bucket(Pid, Bucket, Query) ->
    mapred_bucket(Pid, Bucket, Query, default_timeout(mapred_bucket_timeout)).

%% @spec mapred_bucket(Pid :: pid(),
%%                     Bucket :: bucket(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                     TimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job against a bucket with a timeout
%%      across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
mapred_bucket(Pid, Bucket, Query, Timeout) ->
    mapred_bucket(Pid, Bucket, Query, Timeout, default_timeout(mapred_bucket_call_timeout)).

%% @spec mapred_bucket(Pid :: pid(),
%%                     Bucket :: bucket(),
%%                     Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                     TimeoutMillisecs :: integer() | 'infinity',
%%                     CallTimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a map/reduce job against a bucket with a timeout
%%      across the cluster and local call timeout.
%%      See the map/reduce documentation for explanation of behavior.
mapred_bucket(Pid, Bucket, Query, Timeout, CallTimeout) ->
    case mapred_bucket_stream(Pid, Bucket, Query, self(), Timeout, CallTimeout) of
        {ok, ReqId} ->
            wait_for_mapred(ReqId, Timeout);
        Error ->
            Error
    end.

%% @spec mapred_bucket_stream(Pid :: pid(),
%%                            Bucket :: bucket(),
%%                            Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                            ClientPid :: pid(),
%%                            TimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a streaming map/reduce job against a bucket with a timeout
%%      across the cluster.
%%      See the map/reduce documentation for explanation of behavior.
mapred_bucket_stream(Pid, Bucket, Query, ClientPid, Timeout) ->
    mapred_bucket_stream(Pid, Bucket, Query, ClientPid, Timeout,
                         default_timeout(mapred_bucket_stream_call_timeout)).

%% @spec mapred_bucket_stream(Pid :: pid(),
%%                            Bucket :: bucket(),
%%                            Query :: [riak_kv_mapred_query:mapred_queryterm()],
%%                            ClientPid :: pid(),
%%                            TimeoutMillisecs :: integer() | 'infinity',
%%                            CallTimeoutMillisecs :: integer() | 'infinity') ->
%%       {ok, {ReqId :: term(), MR_FSM_PID :: pid()}} |
%%       {error, Err :: term()}
%% @doc Perform a streaming map/reduce job against a bucket with a server timeout
%%      across the cluster and a call timeout.
%%      See the map/reduce documentation for explanation of behavior.
mapred_bucket_stream(Pid, Bucket, Query, ClientPid, Timeout, CallTimeout) ->
    MapRed = [{'inputs', Bucket},
              {'query', Query},
              {'timeout', Timeout}],
    send_mapred_req(Pid, MapRed, ClientPid, CallTimeout).


%% @doc Execute a search query. This command will return an error 
%%      unless executed against a Riak Search cluster.
%% @spec search(rhc(), bucket(), string()) -> 
%%       {ok, [rhc_mapred:phase_result()]}|{error, term()}
search(Pid, Bucket, SearchQuery) ->
    %% Run a Map/Reduce operation using reduce_identity to get a list
    %% of BKeys.
    IdentityQuery = [{reduce, {modfun, riak_kv_mapreduce, reduce_identity}, none, true}],
    case search(Pid, Bucket, SearchQuery, IdentityQuery,
                default_timeout(search_timeout)) of
        {ok, [{_, Results}]} -> 
            %% Unwrap the results.
            {ok, Results};
        Other -> Other
    end.


%% @doc Execute a search query and feed the results into a map/reduce
%%      query. See {@link rhc_mapred:encode_mapred/2} for details of
%%      the allowed formats for `MRQuery'. This command will return an error 
%%      unless executed against a Riak Search cluster.
%% @spec search(rhc(), bucket(), string(), 
%%       [rhc_mapred:query_part()], integer()) ->
%%       {ok, [rhc_mapred:phase_result()]}|{error, term()}
search(Pid, Bucket, SearchQuery, MRQuery, Timeout) ->
    search(Pid, Bucket, SearchQuery, MRQuery, Timeout,
           default_timeout(search_call_timeout)).


%% @doc Execute a search query and feed the results into a map/reduce
%%      query with a timeout on the call. See {@link rhc_mapred:encode_mapred/2} 
%%      for details of the allowed formats for `MRQuery'. This command will return
%%      an error unless executed against a Riak Search cluster.
%% @spec search(rhc(), bucket(), string(), 
%%       [rhc_mapred:query_part()], integer(), integer()) ->
%%       {ok, [rhc_mapred:phase_result()]}|{error, term()}
search(Pid, Bucket, SearchQuery, MRQuery, Timeout, CallTimeout) ->
    Inputs = {modfun, riak_search, mapred_search, [Bucket, SearchQuery]},
    mapred(Pid, Inputs, MRQuery, Timeout, CallTimeout).


%% @spec default_timeout(OpTimeout) -> timeout()
%% @doc Return the default timeout for an operation if none is provided.
%%      Falls back to the default timeout.
default_timeout(OpTimeout) ->
    case application:get_env(riakc, OpTimeout) of
        {ok, OpTimeout} ->
            OpTimeout;
        undefined ->
            case application:get_env(riakc, timeout) of
                {ok, Timeout} ->
                    Timeout;
                undefined ->
                    ?DEFAULT_TIMEOUT
            end
    end.

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

%% @private
init([Address, Port, Options]) ->
    %% Schedule a reconnect as the first action.  If the server is up then 
    %% the handle_info(reconnect) will run before any requests can be sent.
    State = parse_options(Options, #state{address = Address, 
                                          port = Port,
                                          queue = queue:new()}),
    case State#state.auto_reconnect of
        true ->
            self() ! reconnect,
            {ok, State};
        false ->
            case connect(State) of
                {error, Reason} ->
                    {stop, {tcp, Reason}};
                Ok ->
                    Ok
            end
    end.
        
%% @private
handle_call({req, Msg, Timeout}, From, State) when State#state.sock =:= undefined ->
    case State#state.queue_if_disconnected of
        true ->
            {noreply, queue_request(new_request(Msg, From, Timeout), State)};
        false ->
            {reply, {error, disconnected}, State}
    end;
handle_call({req, Msg, Timeout, Ctx}, From, State) when State#state.sock =:= undefined ->
    case State#state.queue_if_disconnected of
        true ->
            {noreply, queue_request(new_request(Msg, From, Timeout, Ctx), State)};
        false ->
            {reply, {error, disconnected}, State}
    end;
handle_call({req, Msg, Timeout}, From, State) when State#state.active =/= undefined ->
    {noreply, queue_request(new_request(Msg, From, Timeout), State)};
handle_call({req, Msg, Timeout, Ctx}, From, State) when State#state.active =/= undefined ->
    {noreply, queue_request(new_request(Msg, From, Timeout, Ctx), State)};
handle_call({req, Msg, Timeout}, From, State) ->
    {noreply, send_request(new_request(Msg, From, Timeout), State)};
handle_call({req, Msg, Timeout, Ctx}, From, State) ->
    {noreply, send_request(new_request(Msg, From, Timeout, Ctx), State)};
handle_call(is_connected, _From, State) ->
    case State#state.sock of
        undefined ->
            {reply, {false, State#state.failed}, State};
        _ ->
            {reply, true, State}
    end;
handle_call({set_options, Options}, _From, State) ->
    {reply, ok, parse_options(Options, State)};
handle_call(stop, _From, State) ->
    disconnect(State),
    {stop, normal, ok, State}.

%% @private
handle_info({tcp_error, _Socket, Reason}, State) ->
    error_logger:error_msg("PBC client TCP error for ~p:~p - ~p\n",
                           [State#state.address, State#state.port, Reason]),
    disconnect(State);
    
handle_info({tcp_closed, _Socket}, State) ->
    disconnect(State);

%% Make sure the two Sock's match.  If a request timed out, but there was
%% a response queued up behind it we do not want to process it.  Instead
%% it should drop through and be ignored.
handle_info({tcp, Sock, Data}, State=#state{sock = Sock, active = Active}) ->
    [MsgCode|MsgData] = Data,
    Resp = riakc_pb:decode(MsgCode, MsgData),
    case Resp of
        #rpberrorresp{} ->
            NewState1 = maybe_reply(on_error(Active, Resp, State)),
            NewState = dequeue_request(NewState1#state{active = undefined});
        _ ->
            case process_response(Active, Resp, State) of
                {reply, Response, NewState0} ->
                    %% Send reply and get ready for the next request - send the next request
                    %% if one is queued up
                    cancel_req_timer(Active#request.tref),
                    send_caller(Response, NewState0#state.active),
                    NewState = dequeue_request(NewState0#state{active = undefined});
                {pending, NewState0} -> %% Request is still pending - do not queue up a new one
                    NewActive = restart_req_timer(Active),
                    NewState = NewState0#state{active = NewActive}
            end
    end,
    inet:setopts(Sock, [{active, once}]),
    {noreply, NewState};
handle_info({req_timeout, Ref}, State) ->
    case State#state.active of %%
        undefined ->
            {noreply, remove_queued_request(Ref, State)};
        Active ->
            case Ref == Active#request.ref of
                true ->  %% Matches the current operation
                    NewState = maybe_reply(on_timeout(State#state.active, State)),
                    disconnect(NewState#state{active = undefined});
                false ->
                    {noreply, remove_queued_request(Ref, State)}
            end
    end;
handle_info(reconnect, State) ->
    case connect(State) of
        {ok, NewState} ->
            {noreply, dequeue_request(NewState)};
        {error, Reason} ->
            %% Update the failed count and reschedule a reconnection
            NewState = State#state{failed = orddict:update_counter(Reason, 1, State#state.failed)},
            disconnect(NewState)
    end;
handle_info(_, State) ->
    {noreply, State}.            

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% internal functions
%% ====================================================================

%% @private
%% Parse options
parse_options([], State) ->
    %% Once all options are parsed, make sure auto_reconnect is enabled
    %% if queue_if_disconnected is enabled.
    case State#state.queue_if_disconnected of
        true ->
            State#state{auto_reconnect = true};
        _ ->
            State
    end;
parse_options([{connect_timeout, T}|Options], State) when is_integer(T) ->
    parse_options(Options, State#state{connect_timeout = T});
parse_options([{queue_if_disconnected,Bool}|Options], State) when 
      Bool =:= true; Bool =:= false ->
    parse_options(Options, State#state{queue_if_disconnected = Bool});
parse_options([queue_if_disconnected|Options], State) ->
    parse_options([{queue_if_disconnected, true}|Options], State);
parse_options([{auto_reconnect,Bool}|Options], State) when 
      Bool =:= true; Bool =:= false ->
    parse_options(Options, State#state{auto_reconnect = Bool});
parse_options([auto_reconnect|Options], State) ->
    parse_options([{auto_reconnect, true}|Options], State).

maybe_reply({reply, Reply, State}) ->
    Request = State#state.active,
    NewRequest = send_caller(Reply, Request),
    State#state{active = NewRequest};
maybe_reply({noreply, State}) ->
    State.

%% @private
%% Reply to caller - form clause first in case a ReqId/Client was passed
%% in as the context and gen_server:reply hasn't been called yet.
send_caller(Msg, #request{ctx = {ReqId, Client}, 
                          from = undefined}=Request) ->
    Client ! {ReqId, Msg},
    Request;
send_caller(Msg, #request{from = From}=Request) when From /= undefined ->
    gen_server:reply(From, Msg),
    Request#request{from = undefined}.

get_options([], Req) ->
    Req;
get_options([{basic_quorum, BQ} | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{basic_quorum = BQ});
get_options([{notfound_ok, NFOk} | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{notfound_ok = NFOk});
get_options([{r, R} | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{r = normalize_rw_value(R)});
get_options([{pr, PR} | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{pr = normalize_rw_value(PR)});
get_options([{if_modified, VClock} | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{if_modified = VClock});
get_options([head | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{head = true});
get_options([deletedvclock | Rest], Req) ->
    get_options(Rest, Req#rpbgetreq{deletedvclock = true}).


put_options([], Req) ->
    Req;
put_options([{w, W} | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{w = normalize_rw_value(W)});
put_options([{dw, DW} | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{dw = normalize_rw_value(DW)});
put_options([{pw, PW} | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{pw = normalize_rw_value(PW)});
put_options([return_body | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{return_body = 1});
put_options([return_head | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{return_head = true});
put_options([if_not_modified | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{if_not_modified = true});
put_options([if_none_match | Rest], Req) ->
    put_options(Rest, Req#rpbputreq{if_none_match = true}).


delete_options([], Req) ->
    Req;
delete_options([{rw, RW} | Rest], Req) ->
    delete_options(Rest, Req#rpbdelreq{rw = normalize_rw_value(RW)}).

normalize_rw_value(one) -> ?RIAKC_RW_ONE;
normalize_rw_value(quorum) -> ?RIAKC_RW_QUORUM;
normalize_rw_value(all) -> ?RIAKC_RW_ALL;
normalize_rw_value(default) -> ?RIAKC_RW_DEFAULT;
normalize_rw_value(N) -> N.
     

%% Process response from the server - passes back in the request and
%% context the request was issued with.
%% Return noreply if the request is completed, but no reply needed
%%        reply if the request is completed with a reply to the caller
%%        pending if the request has not completed yet (streaming op)
%% @private
-spec process_response(#request{}, rpb_resp(), #state{}) ->
                              {reply, term(), #state{}} |
                              {pending, #state{}}.
process_response(#request{msg = rpbpingreq}, rpbpingresp, State) ->
    {reply, pong, State};
process_response(#request{msg = rpbgetclientidreq},
                 #rpbgetclientidresp{client_id = ClientId}, State) ->
    {reply, {ok, ClientId}, State};
process_response(#request{msg = #rpbsetclientidreq{}},
                 rpbsetclientidresp, State) ->
    {reply, ok, State};
process_response(#request{msg = rpbgetserverinforeq},
                 #rpbgetserverinforesp{node = Node, server_version = ServerVersion}, State) ->
    case Node of
        undefined ->
            NodeInfo = [];
        Node ->
            NodeInfo = [{node, Node}]
    end,
    case ServerVersion of
        undefined ->
            VersionInfo = [];
        ServerVersion ->
            VersionInfo = [{server_version, ServerVersion}]
    end,
    {reply, {ok, NodeInfo++VersionInfo}, State};
process_response(#request{msg = #rpbgetreq{}}, rpbgetresp, State) ->
    %% server just returned the rpbgetresp code - no message was encoded
    {reply, {error, notfound}, State};
process_response(#request{msg = #rpbgetreq{deletedvclock=true}},
                 #rpbgetresp{vclock=VC, content=undefined}, State) ->
    %% server returned a notfound with a vector clock, meaning a tombstone
    {reply, {error, notfound, VC}, State};
process_response(#request{msg = #rpbgetreq{}}, #rpbgetresp{unchanged=true}, State) ->
    %% object was unchanged
    {reply, unchanged, State};
process_response(#request{msg = #rpbgetreq{bucket = Bucket, key = Key}},
                 #rpbgetresp{content = RpbContents, vclock = Vclock}, State) ->
    Contents = riakc_pb:erlify_rpbcontents(RpbContents),
    {reply, {ok, riakc_obj:new_obj(Bucket, Key, Vclock, Contents)}, State};

process_response(#request{msg = #rpbputreq{}},
                 rpbputresp, State) ->
    %% server just returned the rpbputresp code - no message was encoded
    {reply, ok, State};
process_response(#request{ msg = #rpbputreq{}},
                 #rpbputresp{key = Key, content=undefined, vclock=undefined},
                 State) when is_binary(Key) ->
    %% server generated a key and the client didn't request return_body, but
    %% the created key is returned
    {reply, {ok, Key}, State};
process_response(#request{msg = #rpbputreq{bucket = Bucket, key = Key}},
                 #rpbputresp{content = RpbContents, vclock = Vclock,
                     key = NewKey}, State) ->
    Contents = riakc_pb:erlify_rpbcontents(RpbContents),
    ReturnKey = case NewKey of
        undefined -> Key;
        _ -> NewKey
    end,
    {reply, {ok, riakc_obj:new_obj(Bucket, ReturnKey, Vclock, Contents)}, State};

process_response(#request{msg = #rpbdelreq{}},
                 rpbdelresp, State) ->
    %% server just returned the rpbdelresp code - no message was encoded
    {reply, ok, State};

process_response(#request{msg = rpblistbucketsreq},
                 #rpblistbucketsresp{buckets = Buckets}, State) ->
    {reply, {ok, Buckets}, State};

process_response(#request{msg = rpblistbucketsreq},
                 rpblistbucketsresp, State) ->
    %% empty buckets generate an empty message
    {reply, {ok, []}, State};

process_response(#request{msg = #rpblistkeysreq{}}=Request,
                 #rpblistkeysresp{done = Done, keys = Keys}, State) ->
    case Keys of
        undefined ->
            ok;
        _ ->
            %% Have to directly use send_caller as may want to reply with done below.
            send_caller({keys, Keys}, Request)
    end,
    case Done of
        true ->
            {reply, done, State};
        1 ->
            {reply, done, State};
        _ ->
            {pending, State}
    end;

process_response(#request{msg = #rpbgetbucketreq{}},
                 #rpbgetbucketresp{props = PbProps}, State) ->
    Props = riakc_pb:erlify_rpbbucketprops(PbProps),
    {reply, {ok, Props}, State};

process_response(#request{msg = #rpbsetbucketreq{}},
                 rpbsetbucketresp, State) ->
    {reply, ok, State};

process_response(#request{msg = #rpbmapredreq{content_type = ContentType}}=Request,
                 #rpbmapredresp{done = Done, phase=PhaseId, response=Data}, State) ->
    case Data of
        undefined ->
            ok;
        _ ->
            Response = decode_mapred_resp(Data, ContentType),
            send_caller({mapred, PhaseId, Response}, Request)
    end,
    case Done of
        true ->
            {reply, done, State};
        1 ->
            {reply, done, State};
        _ ->
            {pending, State}
    end.

%%
%% Called after sending a message - supports returning a
%% request id for streaming calls
%% @private
after_send(#request{msg = #rpblistkeysreq{}, ctx = {ReqId, _Client}}, State) ->
    {reply, {ok, ReqId}, State};
after_send(#request{msg = #rpbmapredreq{}, ctx = {ReqId, _Client}}, State) ->
    {reply, {ok, ReqId}, State};
after_send(_Request, State) ->
    {noreply, State}.

%% Called on timeout for an operation
%% @private
on_timeout(_Request, State) ->
    {reply, {error, timeout}, State}.
%%
%% Called after receiving an error message - supports reruning
%% an error for streaming calls
%% @private
on_error(_Request, ErrMsg, State) ->
    {reply, fmt_err_msg(ErrMsg), State}.

%% Format the PB encoded error message
fmt_err_msg(ErrMsg) ->
    case ErrMsg#rpberrorresp.errcode of
        Code when Code =:= 1; Code =:= undefined ->
            {error, ErrMsg#rpberrorresp.errmsg};
        Code ->
            {error, {Code, ErrMsg#rpberrorresp.errmsg}}
    end.

%% deliberately crash if the handling an error response after
%% the client has been replied to

%% Common code for sending a single bucket or multiple inputs map/request
%% @private
send_mapred_req(Pid, MapRed, ClientPid, CallTimeout) ->
    ReqMsg = #rpbmapredreq{request = encode_mapred_req(MapRed),
                           content_type = <<"application/x-erlang-binary">>},
    ReqId = mk_reqid(),
    %% Add an extra 100ms to the mapred timeout and use that for the 
    %% socket timeout.  This should give the map/reduce a chance to fail and let us know.
    Timeout = proplists:get_value(timeout, MapRed, default_timeout(mapred_timeout)) + 100,
    gen_server:call(Pid, {req, ReqMsg, Timeout, {ReqId, ClientPid}}, CallTimeout).

%% @private
%% Make a new request that can be sent or queued
new_request(Msg, From, Timeout) ->
    Ref = make_ref(),
    #request{ref = Ref, msg = Msg, from = From, timeout = Timeout,
             tref = create_req_timer(Timeout, Ref)}.
new_request(Msg, From, Timeout, Context) ->
    Ref = make_ref(),
    #request{ref =Ref, msg = Msg, from = From, ctx = Context, timeout = Timeout,
             tref = create_req_timer(Timeout, Ref)}.

%% @private
%% Create a request timer if desired, otherwise return undefined.
create_req_timer(infinity, _Ref) ->
    undefined;
create_req_timer(undefined, _Ref) ->
    undefined;
create_req_timer(Msecs, Ref) ->
    erlang:send_after(Msecs, self(), {req_timeout, Ref}).

%% @private
%% Cancel a request timer made by create_timer/2
cancel_req_timer(undefined) ->
    ok;
cancel_req_timer(Tref) ->
    erlang:cancel_timer(Tref),
    ok.

%% @private
%% Restart a request timer
-spec restart_req_timer(#request{}) -> #request{}.
restart_req_timer(Request) ->
    case Request#request.tref of
        undefined ->
            Request;
        Tref ->
            cancel_req_timer(Tref),
            NewTref = create_req_timer(Request#request.timeout,
                                       Request#request.ref),
            Request#request{tref = NewTref}
    end.

%% @private
%% Connect the socket if disconnected
connect(State) when State#state.sock =:= undefined ->
    #state{address = Address, port = Port, connects = Connects} = State,
    case gen_tcp:connect(Address, Port,
                         [binary, {active, once}, {packet, 4}, {header, 1}],
                         State#state.connect_timeout) of
        {ok, Sock} ->
            {ok, State#state{sock = Sock, connects = Connects+1, 
                             reconnect_interval = ?FIRST_RECONNECT_INTERVAL}};
        Error ->
            Error
    end.

%% @private
%% Disconnect socket if connected
disconnect(State) ->
    %% Tell any pending requests we've disconnected
    case State#state.active of
        undefined ->
            ok;
        Request ->
            send_caller({error, disconnected}, Request)
    end,

    %% Make sure the connection is really closed
    case State#state.sock of
        undefined ->
            ok;
        Sock ->
            gen_tcp:close(Sock)
    end,

    %% Decide whether to reconnect or exit
    NewState = State#state{sock = undefined, active = undefined},
    case State#state.auto_reconnect of
        true ->
            %% Schedule the reconnect message and return state
            erlang:send_after(State#state.reconnect_interval, self(), reconnect),
            {noreply, increase_reconnect_interval(NewState)};
        false ->
            {stop, disconnected, NewState}
    end.
            
%% Double the reconnect interval up to the maximum
increase_reconnect_interval(State) ->
    case State#state.reconnect_interval of
        Interval when Interval < ?MAX_RECONNECT_INTERVAL ->
            NewInterval = lists:min([Interval+Interval,?MAX_RECONNECT_INTERVAL]),
            State#state{reconnect_interval = NewInterval};
        _ ->
            State
    end.

%% Send a request to the server and prepare the state for the response
%% @private
send_request(Request, State) when State#state.active =:= undefined ->
    Pkt = riakc_pb:encode(Request#request.msg),
    gen_tcp:send(State#state.sock, Pkt),
    maybe_reply(after_send(Request, State#state{active = Request})).

%% Queue up a request if one is pending
%% @private
queue_request(Request, State) ->
    State#state{queue = queue:in(Request, State#state.queue)}.

%% Try and dequeue request and send onto the server if one is waiting
%% @private
dequeue_request(State) ->
    case queue:out(State#state.queue) of
        {empty, _} ->
            State;
        {{value, Request}, Q2} ->
            send_request(Request, State#state{queue = Q2})
    end.

%% Remove a queued request by reference - returns same queue if ref not present
%% @private
remove_queued_request(Ref, State) ->
    L = queue:to_list(State#state.queue),
    case lists:keytake(Ref, #request.ref, L) of
        false -> % Ref not queued up
            State;
        {value, Req, L2} ->
            {reply, Reply, NewState} = on_timeout(Req, State),
            send_caller(Reply, Req),
            NewState#state{queue = queue:from_list(L2)}
    end.

%% @private
mk_reqid() -> erlang:phash2(erlang:now()). % only has to be unique per-pid

%% @private
wait_for_listkeys(ReqId, Timeout) ->
    wait_for_listkeys(ReqId,Timeout,[]).
%% @private
wait_for_listkeys(ReqId,Timeout,Acc) ->
    receive
        {ReqId, done} -> {ok, lists:flatten(Acc)};
        {ReqId, {keys,Res}} -> wait_for_listkeys(ReqId,Timeout,[Res|Acc]);
        {ReqId, {error, Reason}} -> {error, Reason}
    after Timeout ->
            {error, {timeout, Acc}}
    end.


%% @private
wait_for_mapred(ReqId, Timeout) ->
    wait_for_mapred(ReqId,Timeout,orddict:new()).
%% @private
wait_for_mapred(ReqId, Timeout, Acc) ->
    receive
        {ReqId, done} -> {ok, orddict:to_list(Acc)};
        {ReqId, {mapred,Phase,Res}} ->
            wait_for_mapred(ReqId,Timeout,orddict:append_list(Phase,Res,Acc));
        {ReqId, {error, Reason}} -> {error, Reason}
    after Timeout ->
            {error, {timeout, orddict:to_list(Acc)}}
    end.


%% Encode the map/reduce request using term to binary
%% @private
encode_mapred_req(Req) ->
    term_to_binary(Req).

%% Decode a partial phase response
%% @private
decode_mapred_resp(Data, <<"application/x-erlang-binary">>) ->
    try
        binary_to_term(Data)
    catch
        _:Error -> % On error, merge in with the other results
            [{error, Error}]
    end.

%% ====================================================================
%% unit tests
%% ====================================================================

%% Tests disabled until they can be prevented from running when included
%% as a dependency.
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("riakc_obj.hrl").

%% Get the test host - check env RIAK_TEST_PB_HOST then env 'RIAK_TEST_HOST'
%% falling back to 127.0.0.1
test_ip() ->
    case os:getenv("RIAK_TEST_PB_HOST") of
        false ->
            case os:getenv("RIAK_TEST_HOST") of
                false ->
                    "127.0.0.1";
                Host ->
                    Host
            end;
        Host ->
            Host
    end.

%% Test port - check env RIAK_TEST_PB_PORT            
test_port() ->
    case os:getenv("RIAK_TEST_PB_PORT") of
        false ->
            8087;
        PortStr ->
            list_to_integer(PortStr)
    end.

%% Riak node under test - used to setup/configure/tweak it for tests
test_riak_node() ->
    case os:getenv("RIAK_TEST_NODE") of
        false ->
            'riak@127.0.0.1';
        NodeStr ->
            list_to_atom(NodeStr)
    end.

%% Node for the eunit node for distributed erlang 
test_eunit_node() ->
    case os:getenv("RIAK_EUNIT_NODE") of
        false ->
            'eunit@127.0.0.1';
        EunitNodeStr ->
            list_to_atom(EunitNodeStr)
    end.

%% Cookie for distributed erlang
test_cookie() ->
    case os:getenv("RIAK_TEST_COOKIE") of
        false ->
            'riak';
        CookieStr ->
            list_to_atom(CookieStr)
    end.
    

reset_riak() ->
    ?assertEqual(ok, maybe_start_network()),

    %% Until there is a good way to empty the vnodes, require the
    %% test to run with ETS and kill the vnode master/sup to empty all the ETS tables
    %% and the ring manager to remove any bucket properties
    ok = rpc:call(test_riak_node(), application, set_env, [riak_kv, storage_backend, riak_kv_ets_backend]),

    %% Restart the vnodes so they come up with ETS
    ok = supervisor:terminate_child({riak_kv_sup, test_riak_node()}, riak_kv_vnode_master),
    ok = supervisor:terminate_child({riak_core_sup, test_riak_node()}, riak_core_vnode_sup),
    {ok, _} = supervisor:restart_child({riak_core_sup, test_riak_node()}, riak_core_vnode_sup),
    {ok, _} = supervisor:restart_child({riak_kv_sup, test_riak_node()}, riak_kv_vnode_master),
    
    %% Clear the map/reduce cache 
    ok = rpc:call(test_riak_node(), riak_kv_mapred_cache, clear, []),

    %% Now reset the ring so bucket properties are default
    Ring = rpc:call(test_riak_node(), riak_core_ring, fresh, []),
    ok = rpc:call(test_riak_node(), riak_core_ring_manager, set_my_ring, [Ring]).


riak_pb_listener_pid() ->
    Children = supervisor:which_children({riak_kv_sup, test_riak_node()}),
    hd([Pid || {Mod,Pid,_,_} <- Children, Mod == riak_kv_pb_listener]).

pause_riak_pb_listener() ->
    Pid = riak_pb_listener_pid(),
    rpc:call(test_riak_node(), sys, suspend, [Pid]).

resume_riak_pb_listener() ->
    Pid = riak_pb_listener_pid(),
    rpc:call(test_riak_node(), sys, resume, [Pid]).

kill_riak_pb_sockets() ->
    case supervisor:which_children({riak_kv_pb_socket_sup, test_riak_node()}) of
        [] ->
            ok;
        Children ->
            Pids = [Pid || {_,Pid,_,_} <- Children],
            [rpc:call(test_riak_node(), erlang, exit, [Pid, kill]) || Pid <- Pids],
            erlang:yield(),
            kill_riak_pb_sockets()
    end.

maybe_start_network() ->
    %% Try to spin up net_kernel
    os:cmd("epmd -daemon"),
    case net_kernel:start([test_eunit_node()]) of
        {ok, _} ->
            erlang:set_cookie(test_riak_node(), test_cookie()),
            ok;
        {error, {already_started, _}} ->
            ok;
        X ->
            X
    end.

bad_connect_test() ->
    %% Start with an unlikely port number
    ?assertEqual({error, {tcp, econnrefused}}, start({127,0,0,1}, 65535)).

queue_disconnected_test() ->
    %% Start with an unlikely port number
    {ok, Pid} = start({127,0,0,1}, 65535, [queue_if_disconnected]),
    ?assertEqual({error, timeout}, ping(Pid, 10)),    
    ?assertEqual({error, timeout}, list_keys(Pid, <<"b">>, 10)),
    stop(Pid).

auto_reconnect_bad_connect_test() ->
    %% Start with an unlikely port number
    {ok, Pid} = start({127,0,0,1}, 65535, [auto_reconnect]),
    ?assertEqual({false, [{econnrefused,1}]}, is_connected(Pid)),
    ?assertEqual({error, disconnected}, ping(Pid)),
    ?assertEqual({error, disconnected}, list_keys(Pid, <<"b">>)),
    stop(Pid).

server_closes_socket_test() ->
    %% Set up a dummy socket to send requests on
    {ok, Listen} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Pid} = start("127.0.0.1", Port),
    {ok, Sock} = gen_tcp:accept(Listen),
    ?assertMatch(true, is_connected(Pid)),

    %% Send a ping request in another process so the test doesn't block
    Self = self(),
    spawn(fun() -> Self ! ping(Pid, infinity) end),

    %% Make sure request received then close the socket
    {ok, _ReqMsg} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(Listen),
    receive
        Msg1 -> % result of ping from spawned process above
            ?assertEqual({error, disconnected}, Msg1)
    end,
    %% Wait for spawned process to exit
    Mref = erlang:monitor(process, Pid),
    receive
        Msg2 ->
            ?assertMatch({'DOWN', Mref, process, _, _}, Msg2)
    end.

auto_reconnect_server_closes_socket_test() ->
    %% Set up a dummy socket to send requests on
    {ok, Listen} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Pid} = start_link("127.0.0.1", Port, [auto_reconnect]),
    {ok, Sock} = gen_tcp:accept(Listen),
    ?assertMatch(true, is_connected(Pid)),

    %% Send a ping request in another process so the test doesn't block
    Self = self(),
    spawn(fun() -> Self ! ping(Pid, infinity) end),

    %% Make sure request received then close the socket
    {ok, _ReqMsg} = gen_tcp:recv(Sock, 0),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(Listen),
    receive
        Msg ->
            ?assertEqual({error, disconnected}, Msg)
    end,
    %% Server will not have had a chance to reconnect yet, reason counters empty.
    ?assertMatch({false, []}, is_connected(Pid)),
    stop(Pid).

dead_socket_pid_returns_to_caller_test() ->
    %% Set up a dummy socket to send requests on
    {ok, Listen} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, Port} = inet:port(Listen),
    {ok, Pid} = start("127.0.0.1", Port),
    {ok, Sock} = gen_tcp:accept(Listen),
    ?assertMatch(true, is_connected(Pid)),

    %% Send a ping request in another process so the test doesn't block
    Self = self(),
    spawn(fun() -> Self ! (catch ping(Pid, infinity)) end),

    %% Make sure request received then kill the process
    {ok, _ReqMsg} = gen_tcp:recv(Sock, 0),
    exit(Pid, kill),
    receive
        Msg ->
            ?assertMatch({'EXIT', {killed, _}}, Msg)
    end,
    %% Cleanup
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(Listen).

pb_socket_test_() ->
    {setup,
     fun() ->
             %% Grab the riakclient_pb.proto file
             code:add_pathz("../ebin"),
             ok = maybe_start_network()
     end,
     fun(_) ->
             net_kernel:stop()
     end,
     {generator,
     fun() ->
             case catch net_adm:ping(test_riak_node()) of
                 pong ->
                     live_node_tests();
                 _ ->
                     [] %% {skipped, need_live_server};
             end
     end}}.


%% Check the reconnect interval increases up to the max and sticks there
increase_reconnect_interval_test() ->
    increase_reconnect_interval_test(#state{}).

increase_reconnect_interval_test(State) ->
    CurrInterval = State#state.reconnect_interval,
    NextState = increase_reconnect_interval(State),
    case NextState#state.reconnect_interval of
        ?MAX_RECONNECT_INTERVAL ->
            FinalState = increase_reconnect_interval(NextState),
            ?assertEqual(?MAX_RECONNECT_INTERVAL, FinalState#state.reconnect_interval);
        NextInterval->
            ?assert(NextInterval > CurrInterval),
            increase_reconnect_interval_test(NextState)
    end.

%%
%% Tests to run against a live node - NB the node gets reconfigured and generally messed with
%%
live_node_tests() ->
    [{"ping",
      ?_test( begin
                  {ok, Pid} = start_link(test_ip(), test_port()),
                  ?assertEqual(pong, ?MODULE:ping(Pid)),
                  ?assertEqual(true, is_connected(Pid)),
                  stop(Pid)
              end)},
     {"reconnect test",
      ?_test( begin
                  %% Make sure originally there
                  {ok, Pid} = start_link(test_ip(), test_port()),
                  
                  %% Change the options to allow reconnection/queueing
                  set_options(Pid, [queue_if_disconnected]),
                  
                  %% Kill the socket
                  kill_riak_pb_sockets(),
                  ?assertEqual(pong, ?MODULE:ping(Pid)),
                  stop(Pid)
              end)},

     {"set client id",
      ?_test(
         begin
             {ok, Pid} = start_link(test_ip(), test_port()),
             {ok, <<OrigId:32>>} = ?MODULE:get_client_id(Pid),

             NewId = <<(OrigId+1):32>>,
             ok = ?MODULE:set_client_id(Pid, NewId),
             {ok, NewId} = ?MODULE:get_client_id(Pid)
         end)},

     {"version",
      ?_test(
         begin
             {ok, Pid} = start_link(test_ip(), test_port()),
             {ok, ServerInfo} = ?MODULE:get_server_info(Pid),
             [{node, _}, {server_version, _}] = lists:sort(ServerInfo)
         end)},

     {"get_should_read_put_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O0 = riakc_obj:new(<<"b">>, <<"k">>),
                 O = riakc_obj:update_value(O0, <<"v">>),
                 {ok, PO} = ?MODULE:put(Pid, O, [return_body]),
                 {ok, GO} = ?MODULE:get(Pid, <<"b">>, <<"k">>),
                 ?assertEqual(riakc_obj:get_contents(PO), riakc_obj:get_contents(GO))
             end)},

     {"get should read put with timeout",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O0 = riakc_obj:new(<<"b">>, <<"k">>),
                 O = riakc_obj:update_value(O0, <<"v">>),
                 {ok, PO} = ?MODULE:put(Pid, O, [{w, 1}, {dw, 1}, return_body]),
                 {ok, GO} = ?MODULE:get(Pid, <<"b">>, <<"k">>, 500),
                 ?assertEqual(riakc_obj:get_contents(PO), riakc_obj:get_contents(GO))
             end)},

     {"get should read put with options",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O0 = riakc_obj:new(<<"b">>, <<"k">>),
                 O = riakc_obj:update_value(O0, <<"v">>),
                 {ok, PO} = ?MODULE:put(Pid, O, [{w, 1}, {dw, 1}, return_body]),
                 {ok, GO} = ?MODULE:get(Pid, <<"b">>, <<"k">>, [{r, 1}]),
                 ?assertEqual(riakc_obj:get_contents(PO), riakc_obj:get_contents(GO))
             end)},

     {"put and delete with timeout",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 PO = riakc_obj:new(<<"b">>, <<"puttimeouttest">>, <<"value">>),
                 ok = ?MODULE:put(Pid, PO, 500),
                 {ok, GO} = ?MODULE:get(Pid, <<"b">>, <<"puttimeouttest">>, 500),
                 ?assertEqual(<<"value">>, riakc_obj:get_value(GO)),
                 ok = ?MODULE:delete(Pid, <<"b">>, <<"puttimeouttest">>, 500),
                 {error, notfound} = ?MODULE:get(Pid, <<"b">>, <<"puttimeouttest">>)
             end)},

     {"update_should_change_value_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O0 = riakc_obj:new(<<"b">>, <<"k">>),
                 O = riakc_obj:update_value(O0, <<"v">>),
                 {ok, PO} = ?MODULE:put(Pid, O, [return_body]),
                 PO2 = riakc_obj:update_value(PO, <<"v2">>),
                 ok = ?MODULE:put(Pid, PO2),
                 {ok, GO} = ?MODULE:get(Pid, <<"b">>, <<"k">>),
                 ?assertEqual(<<"v2">>, riakc_obj:get_value(GO))
             end)},

     {"key_should_be_missing_after_delete_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 %% Put key/value
                 O0 = riakc_obj:new(<<"b">>, <<"k">>),
                 O = riakc_obj:update_value(O0, <<"v">>),
                 {ok, _PO} = ?MODULE:put(Pid, O, [return_body]),
                 %% Prove it really got stored
                 {ok, GO1} = ?MODULE:get(Pid, <<"b">>, <<"k">>),
                 ?assertEqual(<<"v">>, riakc_obj:get_value(GO1)),
                 %% Delete and check no longer found
                 ok = ?MODULE:delete(Pid, <<"b">>, <<"k">>),
                 {error, notfound} = ?MODULE:get(Pid, <<"b">>, <<"k">>)
             end)},

    {"delete missing key test",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                  %% Delete and check no longer found
                 ok = ?MODULE:delete(Pid, <<"notabucket">>, <<"k">>, [{rw, 1}]),
                 {error, notfound} = ?MODULE:get(Pid, <<"notabucket">>, <<"k">>)
             end)},

     {"empty_list_buckets_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 ?assertEqual({ok, []}, ?MODULE:list_buckets(Pid))
             end)},

     {"list_buckets_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Bs = lists:sort([list_to_binary(["b"] ++ integer_to_list(N)) || N <- lists:seq(1, 10)]),
                 F = fun(B) ->
                             O=riakc_obj:new(B, <<"key">>),
                             ?MODULE:put(Pid, riakc_obj:update_value(O, <<"val">>))
                     end,
                 [F(B) || B <- Bs],
                 {ok, LBs} = ?MODULE:list_buckets(Pid),
                 ?assertEqual(Bs, lists:sort(LBs))
             end)},

     {"list_keys_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Bucket = <<"listkeys">>,
                 Ks = lists:sort([list_to_binary(integer_to_list(N)) || N <- lists:seq(1, 10)]),
                 F = fun(K) ->
                             O=riakc_obj:new(Bucket, K),
                             ?MODULE:put(Pid, riakc_obj:update_value(O, <<"val">>))
                     end,
                 [F(K) || K <- Ks],
                 {ok, LKs} = ?MODULE:list_keys(Pid, Bucket),
                 ?assertEqual(Ks, lists:sort(LKs)),

                 %% Make sure it works with an infinite timeout (will reset the timeout
                 %% timer after each packet)
                 {ok, LKs2} = ?MODULE:list_keys(Pid, Bucket, infinity),
                 ?assertEqual(Ks, lists:sort(LKs2))
             end)},

     {"get bucket properties test",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 {ok, Props} = get_bucket(Pid, <<"b">>),
                 ?assertEqual([{allow_mult,false},
                               {n_val,3}],
                              lists:sort(Props))
             end)},

     {"get bucket properties test",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 ok = set_bucket(Pid, <<"b">>, [{n_val, 2}, {allow_mult, true}]),
                 {ok, Props} = get_bucket(Pid, <<"b">>),
                 ?assertEqual([{allow_mult,true},
                               {n_val,2}],
                              lists:sort(Props))
             end)},

     {"allow_mult should allow dupes",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid1} = start_link(test_ip(), test_port()),
                 {ok, Pid2} = start_link(test_ip(), test_port()),
                 ok = set_bucket(Pid1, <<"multibucket">>, [{allow_mult, true}]),
                 ?MODULE:delete(Pid1, <<"multibucket">>, <<"foo">>),
                 {error, notfound} = ?MODULE:get(Pid1, <<"multibucket">>, <<"foo">>),
                 O = riakc_obj:new(<<"multibucket">>, <<"foo">>),
                 O1 = riakc_obj:update_value(O, <<"pid1">>),
                 O2 = riakc_obj:update_value(O, <<"pid2">>),
                 ok = ?MODULE:put(Pid1, O1),

                 ok = ?MODULE:put(Pid2, O2),
                 {ok, O3} = ?MODULE:get(Pid1, <<"multibucket">>, <<"foo">>),
                 ?assertEqual([<<"pid1">>, <<"pid2">>], lists:sort(riakc_obj:get_values(O3))),
                 O4 = riakc_obj:update_value(riakc_obj:select_sibling(1, O3), <<"resolved">>),
                 ok = ?MODULE:put(Pid1, O4),
                 {ok, GO} = ?MODULE:get(Pid1, <<"multibucket">>, <<"foo">>),
                 ?assertEqual([<<"resolved">>], lists:sort(riakc_obj:get_values(GO))),
                 ?MODULE:delete(Pid1, <<"multibucket">>, <<"foo">>)
             end)},

     {"update object test", 
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O0 = riakc_obj:new(<<"b">>, <<"k">>, <<"d">>),
                 io:format("O0: ~p\n", [O0]),
                 {ok, O1} = riakc_pb_socket:put(Pid, O0, [return_body]),
                 io:format("O1: ~p\n", [O1]),
                 M1 = riakc_obj:get_metadata(O1),
                 M2 = dict:store(?MD_LINKS, [{{<<"b">>, <<"k1">>}, <<"t1">>}], M1),
                 O2 = riakc_obj:update_metadata(O1, M2),
                 riakc_pb_socket:put(Pid, O2)
             end)},

     {"queue test",
      ?_test(begin
                 %% Would really like this in a nested {setup, blah} structure
                 %% but eunit does not allow
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 pause_riak_pb_listener(),
                 Me = self(),
                 %% this request will block as
                 spawn(fun() -> Me ! {1, ping(Pid)} end),
                 %% this request should be queued as socket will not be created
                 spawn(fun() -> Me ! {2, ping(Pid)} end),
                 resume_riak_pb_listener(),
                 receive {1,Ping1} -> ?assertEqual(Ping1, pong) end,
                 receive {2,Ping2} -> ?assertEqual(Ping2, pong) end
             end)},

    {"timeout queue test",
      ?_test(begin
                 %% Would really like this in a nested {setup, blah} structure
                 %% but eunit does not allow
                 pause_riak_pb_listener(),
                 {ok, Pid} = start_link(test_ip(), test_port(), [queue_if_disconnected]),
                 Me = self(),
                 %% this request will block as
                 spawn(fun() -> Me ! {1, ping(Pid, 0)} end),
                 %% this request should be queued as socket will not be created
                 spawn(fun() -> Me ! {2, ping(Pid, 0)},  Me ! running end),
                 receive running -> ok end,
                 resume_riak_pb_listener(),
                 receive {1,Ping1} -> ?assertEqual({error, timeout}, Ping1) end,
                 receive {2,Ping2} -> ?assertEqual({error, timeout}, Ping2) end
             end)},

    {"ignore stale tref test",
      ?_test(begin
                 %% Would really like this in a nested {setup, blah} structure
                 %% but eunit does not allow
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Pid ! {req_timeout, make_ref()},
                 ?assertEqual(pong, ping(Pid))
             end)},

   {"infinite timeout ping test",
      ?_test(begin
                 %% Would really like this in a nested {setup, blah} structure
                 %% but eunit does not allow
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 ?assertEqual(pong, ping(Pid, infinity)),
                 ?assertEqual(pong, ping(Pid, undefined))
             end)},

     {"javascript_source_map_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 B = <<"bucket">>,
                 K = <<"foo">>,
                 O=riakc_obj:new(B, K),
                 ?MODULE:put(Pid, riakc_obj:update_value(O, <<"2">>, "application/json")),

                 ?assertEqual({ok, [{0, [2]}]},
                              ?MODULE:mapred(Pid,
                                             [{B, K}],
                                             [{map, {jsanon, <<"function (v) { return [JSON.parse(v.values[0].data)]; }">>},
                                               undefined, true}]))
             end)},

     {"javascript_named_map_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 B = <<"bucket">>,
                 K = <<"foo">>,
                 O=riakc_obj:new(B, K),
                 ?MODULE:put(Pid, riakc_obj:update_value(O, <<"99">>, "application/json")),

                 ?assertEqual({ok, [{0, [99]}]},
                              ?MODULE:mapred(Pid,
                                             [{B, K}],
                                             [{map, {jsfun, <<"Riak.mapValuesJson">>},
                                               undefined, true}]))
             end)},

     {"javascript_source_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Store = fun({K,V}) ->
                                 O=riakc_obj:new(<<"bucket">>, K),
                                 ?MODULE:put(Pid,riakc_obj:update_value(O, V, "application/json"))
                         end,
                 [Store(KV) || KV <- [{<<"foo">>, <<"2">>},
                                      {<<"bar">>, <<"3">>},
                                      {<<"baz">>, <<"4">>}]],

                 ?assertEqual({ok, [{1, [3]}]},
                              ?MODULE:mapred(Pid,
                                             [{<<"bucket">>, <<"foo">>},
                                              {<<"bucket">>, <<"bar">>},
                                              {<<"bucket">>, <<"baz">>}],
                                             [{map, {jsanon, <<"function (v) { return [1]; }">>},
                                               undefined, false},
                                              {reduce, {jsanon, 
                                                        <<"function(v) {
                                                             total = v.reduce(
                                                               function(prev,curr,idx,array) {
                                                                 return prev+curr;
                                                               }, 0);
                                                             return [total];
                                                           }">>},
                                               undefined, true}]))
             end)},

     {"javascript_named_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Store = fun({K,V}) ->
                                 O=riakc_obj:new(<<"bucket">>, K),
                                 ?MODULE:put(Pid,riakc_obj:update_value(O, V, "application/json"))
                         end,
                 [Store(KV) || KV <- [{<<"foo">>, <<"2">>},
                                      {<<"bar">>, <<"3">>},
                                      {<<"baz">>, <<"4">>}]],

                 ?assertEqual({ok, [{1, [9]}]},
                              ?MODULE:mapred(Pid,
                                             [{<<"bucket">>, <<"foo">>},
                                              {<<"bucket">>, <<"bar">>},
                                              {<<"bucket">>, <<"baz">>}],
                                             [{map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, false},
                                              {reduce, {jsfun, <<"Riak.reduceSum">>}, undefined, true}]))
             end)},

     {"javascript_bucket_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Store = fun({K,V}) ->
                                 O=riakc_obj:new(<<"bucket">>, K),
                                 ?MODULE:put(Pid,riakc_obj:update_value(O, V, "application/json"))
                         end,
                 [Store(KV) || KV <- [{<<"foo">>, <<"2">>},
                                      {<<"bar">>, <<"3">>},
                                      {<<"baz">>, <<"4">>}]],

                 ?assertEqual({ok, [{1, [9]}]},
                              ?MODULE:mapred_bucket(Pid, <<"bucket">>,
                                                    [{map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, false},
                                                     {reduce, {jsfun, <<"Riak.reduceSum">>}, undefined, true}]))
             end)},

     {"javascript_arg_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 O=riakc_obj:new(<<"bucket">>, <<"foo">>),
                 ?MODULE:put(Pid, riakc_obj:update_value(O, <<"2">>, "application/json")),
                 ?assertEqual({ok, [{1, [10]}]},
                              ?MODULE:mapred(Pid,
                                             [{{<<"bucket">>, <<"foo">>}, 5},
                                              {{<<"bucket">>, <<"foo">>}, 10},
                                              {{<<"bucket">>, <<"foo">>}, 15},
                                              {{<<"bucket">>, <<"foo">>}, -15},
                                              {{<<"bucket">>, <<"foo">>}, -5}],
                                             [{map, {jsanon, <<"function(v, arg) { return [arg]; }">>},
                                               undefined, false},
                                              {reduce, {jsfun, <<"Riak.reduceSum">>}, undefined, true}]))
             end)},

     {"erlang_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Store = fun({K,V}) ->
                                 O=riakc_obj:new(<<"bucket">>, K),
                                 ?MODULE:put(Pid,riakc_obj:update_value(O, V, "application/json"))
                         end,
                 [Store(KV) || KV <- [{<<"foo">>, <<"2">>},
                                      {<<"bar">>, <<"3">>},
                                      {<<"baz">>, <<"4">>}]],

                 {ok, [{1, Results}]} = ?MODULE:mapred(Pid,
                                                       [{<<"bucket">>, <<"foo">>},
                                                        {<<"bucket">>, <<"bar">>},
                                                        {<<"bucket">>, <<"baz">>}],
                                                       [{map, {modfun, riak_kv_mapreduce,
                                                               map_object_value},
                                                         undefined, false},
                                                        {reduce, {modfun, riak_kv_mapreduce,
                                                                  reduce_set_union},
                                                         undefined, true}]),
                 ?assertEqual([<<"2">>, <<"3">>, <<"4">>], lists:sort(Results))
             end)},
     {"missing_key_erlang_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = {ok, Pid} = start_link(test_ip(), test_port()),
                 {ok, Results} = ?MODULE:mapred(Pid, [{<<"bucket">>, <<"foo">>},
                                                      {<<"bucket">>, <<"bar">>},
                                                      {<<"bucket">>, <<"baz">>}],
                                                [{map, {modfun, riak_kv_mapreduce,
                                                        map_object_value},
                                                  <<"include_notfound">>, false},
                                                 {reduce, {modfun, riak_kv_mapreduce,
                                                           reduce_set_union},
                                                  undefined, true}]),
                 [{1, [{error, notfound}|_]}] = Results end)},
     {"missing_key_javascript_map_reduce_test()",
      ?_test(begin
                 reset_riak(),
                 {ok, Pid} = {ok, Pid} = start_link(test_ip(), test_port()),
                 {ok, Results} = ?MODULE:mapred(Pid, [{<<"bucket">>, <<"foo">>},
                                                      {<<"bucket">>, <<"bar">>},
                                                      {<<"bucket">>, <<"baz">>}],
                                                [{map, {jsfun, <<"Riak.mapValuesJson">>},
                                                  undefined, false},
                                                 {reduce, {jsfun, <<"Riak.reduceSort">>},
                                                  undefined, true}]),
                 [{1, [{not_found, {_, _},<<"undefined">>}|_]}] = Results end)},
     {"map reduce bad inputs",
      ?_test(begin
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 ?assertEqual({error, <<"{inputs,{\"Inputs must be a binary bucket, a tuple of bucket and key-filters, a list of target tuples, or a modfun tuple:\",\n         undefined}}">>},
                              ?MODULE:mapred(Pid, undefined,
                                             [{map, {jsfun, <<"Riak.mapValuesJson">>},
                                               undefined, false},
                                              {reduce, {jsfun, <<"Riak.reduceSum">>},
                                               undefined, true}]))
             end)},
     {"map reduce bad input keys",
      ?_test(begin
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Res = ?MODULE:mapred(Pid, [<<"b">>], % no {B,K} tuple
                                      [{map, {jsfun, <<"Riak.mapValuesJson">>},
                                        undefined, false},
                                       {reduce, {jsfun, <<"Riak.reduceSum">>},
                                        undefined, true}]),
                 ?assertEqual({error,<<"{inputs,{\"Inputs target tuples must be {B,K} or {{B,K},KeyData}:\",[<<\"b\">>]}}">>},
                              Res)
             end)},
     {"map reduce bad query",
      ?_test(begin
                 {ok, Pid} = start_link(test_ip(), test_port()),
                 Res = ?MODULE:mapred(Pid, [{<<"b">>,<<"k">>}], % no {B,K} tuple
                                      undefined),
                 ?assertEqual({error,<<"{'query',{\"Query takes a list of step tuples\",undefined}}">>},
                              Res)
             end)},
     {"get should convert erlang terms",
      ?_test(begin
                 reset_riak(),
                 TestNode = test_riak_node(),
                 MyBin = <<"some binary">>,
                 MyTerm = [<<"b">>,<<"a_term">>,{some_term, ['full', "of", 123, 654.321]}],
                 BinObj = rpc:call(TestNode, riak_object, new,
                                   [<<"b">>, <<"a_bin">>, MyBin]),
                 TermObj = rpc:call(TestNode, riak_object, new,
                                    [<<"b">>, <<"a_term">>, MyTerm]),
                 {ok, C} = rpc:call(TestNode, riak, local_client, []),
                 %% parameterized module trickery - stick it as the last argument
                 ok = rpc:call(TestNode, riak_client, put, [BinObj, 1, C]),
                 ok = rpc:call(TestNode, riak_client, put, [TermObj, 1, C]),

                 {ok, Pid} = start_link(test_ip(), test_port()),
                 {ok, GotBinObj} = ?MODULE:get(Pid, <<"b">>, <<"a_bin">>),
                 {ok, GotTermObj} = ?MODULE:get(Pid, <<"b">>, <<"a_term">>),
  
                 ?assertEqual(riakc_obj:get_value(GotBinObj), MyBin),
                 ?assertEqual(riakc_obj:get_content_type(GotTermObj),
                              "application/x-erlang-binary"),
                 ?assertEqual(binary_to_term(riakc_obj:get_value(GotTermObj)), MyTerm)
             end)},
     {"putting without a key should generate one",
         ?_test(begin
                     reset_riak(),
                     {ok, Pid} = start_link(test_ip(), test_port()),
                     PO = riakc_obj:new(<<"b">>, undefined, <<"value">>),
                     Res1 = ?MODULE:put(Pid, PO),
                     Res2 = ?MODULE:put(Pid, PO),
                     ?assertMatch({ok, _Key}, Res1),
                     % Make sure the same key isn't generated twice
                     ?assert(Res1 =/= Res2)
             end)},
     {"putting without a key should generate one with return_body",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, undefined, <<"value">>),
                    {ok, Obj1} = ?MODULE:put(Pid, PO, [return_body]),
                    {ok, Obj2} = ?MODULE:put(Pid, PO, [return_body]),
                    %% Make sure the same key isn't generated twice
                    ?assertEqual(element(1, Obj1), riakc_obj),
                    ?assertEqual(element(1, Obj2), riakc_obj),
                    ?assert(riakc_obj:key(Obj1) /= riakc_obj:key(Obj2))
             end)},
     {"conditional gets should return unchanged if the vclock matches",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    ?MODULE:put(Pid, PO),
                    {ok, Obj} = ?MODULE:get(Pid, <<"b">>, <<"key">>),
                    VClock = riakc_obj:vclock(Obj),
                    %% object hasn't changed
                    ?assertEqual(unchanged, ?MODULE:get(Pid, <<"b">>, <<"key">>,
                            [{if_modified, VClock}])),
                    %% change the object and make sure unchanged isn't returned
                    P1 = riakc_obj:update_value(Obj, <<"newvalue">>),
                    ?MODULE:put(Pid, P1),
                    ?assertMatch({ok, _}, ?MODULE:get(Pid, <<"b">>, <<"key">>,
                            [{if_modified, VClock}]))
             end)},
     {"the head get option should return the object metadata without the value",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    ?MODULE:put(Pid, PO),
                    {ok, Obj} = ?MODULE:get(Pid, <<"b">>, <<"key">>, [head]),
                    ?assertEqual(<<>>, riakc_obj:get_value(Obj)),
                    {ok, Obj2} = ?MODULE:get(Pid, <<"b">>, <<"key">>, []),
                    ?assertEqual(<<"value">>, riakc_obj:get_value(Obj2))
             end)},
     {"conditional put should allow you to avoid overwriting a value if it already exists",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    ?assertEqual(ok, ?MODULE:put(Pid, PO, [if_none_match])),
                    ?assertEqual({error, <<"match_found">>}, ?MODULE:put(Pid, PO, [if_none_match]))
             end)},
     {"conditional put should allow you to avoid overwriting a value if its been updated",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    {ok, Obj} = ?MODULE:put(Pid, PO, [return_body]),
                    Obj2 = riakc_obj:update_value(Obj, <<"newvalue">>),
                    ?assertEqual(ok, ?MODULE:put(Pid, Obj2, [if_not_modified])),
                    ?assertEqual({error, <<"modified">>}, ?MODULE:put(Pid, Obj2, [if_not_modified]))
             end)},
     {"if_not_modified should fail if the object is not found",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    ?assertEqual({error, <<"notfound">>}, ?MODULE:put(Pid, PO, [if_not_modified]))
             end)},
     {"return_head should empty out the value in the riak object",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    {ok, Obj} = ?MODULE:put(Pid, PO, [return_head]),
                    ?assertEqual(<<>>, riakc_obj:get_value(Obj))
             end)},
     {"return_head should empty out all values when there's siblings",
         ?_test(begin
                    reset_riak(),
                    {ok, Pid} = start_link(test_ip(), test_port()),
                    ok = set_bucket(Pid, <<"b">>, [{allow_mult, true}]),
                    PO = riakc_obj:new(<<"b">>, <<"key">>, <<"value">>),
                    {ok, Obj} = ?MODULE:put(Pid, PO, [return_head]),
                    ?assertEqual(<<>>, riakc_obj:get_value(Obj)),
                    {ok, Obj2} = ?MODULE:put(Pid, PO, [return_head]),
                    ?assertEqual([<<>>, <<>>], riakc_obj:get_values(Obj2))
             end)}

     ].

-endif.
