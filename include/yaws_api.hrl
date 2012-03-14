%%%----------------------------------------------------------------------
%%% File    : yaws_api.hrl
%%% Author  : Claes Wikstrom <klacke@hyber.org>
%%% Purpose : 
%%% Created : 24 Jan 2002 by Claes Wikstrom <klacke@hyber.org>
%%%----------------------------------------------------------------------

-author('klacke@hyber.org').

-record(arg, {
          clisock,        %% the socket leading to the peer client
          client_ip_port, %% {ClientIp, ClientPort} tuple
          headers,        %% headers
          req,            %% request
          clidata,        %% The client data (as a binary in POST requests)
          server_path,    %% The normalized server path 
                          %% (pre-querystring part of URI)
          querydata,      %% For URIs of the form ...?querydata 
                          %%  equiv of cgi QUERY_STRING
          appmoddata,     %% (deprecated - use pathinfo instead) the remainder 
                          %% of the path leading up to the query
          docroot,        %% Physical base location of data for this request
          docroot_mount,  %% virtual directory e.g /myapp/ that the docroot
                          %%  refers to.
          fullpath,       %% full deep path to yaws file
          cont,           %% Continuation for chunked multipart uploads
          state,          %% State for use by users of the out/1 callback
          pid,            %% pid of the yaws worker process
          opaque,         %% useful to pass static data
          appmod_prepath, %% (deprecated - use prepath instead) path in front 
                          %%of: <appmod><appmoddata>
          prepath,        %% Path prior to 'dynamic' segment of URI. 
                          %%  ie http://some.host/<prepath>/<script-point>/d/e 
                          %% where <script-point> is an appmod mount point, 
                          %% or .yaws,.php,.cgi,.fcgi etc script file.
          pathinfo        %% Set to '/d/e' when calling c.yaws for the request 
                          %% http://some.host/a/b/c.yaws/d/e
                          %%  equiv of cgi PATH_INFO
         }).


-record(http_request, {method,
                       path,
                       version}).

-record(http_response, {version,
                        status,
                        phrase}).

-record(rewrite_response, {status,
                           headers = [],
                           content = <<>>}).

-record(headers, {
          connection,
          accept,
          host,
          if_modified_since,
          if_match,
          if_none_match,
          if_range,
          if_unmodified_since,
          range,
          referer,
          user_agent,
          accept_ranges,
          cookie = [],
          keep_alive,
          location,
          content_length,
          content_type,
          content_encoding,
          authorization,
          transfer_encoding,
          x_forwarded_for,
          other = []   %% misc other headers
         }).




-record(url,
        {scheme,          %% undefined means not set
         host,            %% undefined means not set
         port,            %% undefined means not set
         path = [],
         querypart = []}).


-record(setcookie,{
            key,           
            value,         
            quoted,        
            comment,
            comment_url,
            discard,
            domain,
            max_age,
            expires,
            path,
            port,
            secure,
            version}).


-record(redir_self, {
          host,        %% string() - our own host
          scheme,      %% http | https
          scheme_str,  %% "https://"  | "http://"
          port,        %% integer()  - our own port
          port_str     %% "" | ":<int>" - the optional port part
                       %%                 to append to the url
         }).

%% Corresponds to the frame sections as in
%% http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-08#section-4
%% plus 'data' and 'ws_state'
-record(ws_frame_info, {
	  fin,
	  rsv,
	  opcode,
	  masked,
	  masking_key,
	  length,
	  payload,
	  data,        %% The unmasked payload. Makes payload redundant.
	  ws_state     %% The ws_state after unframing this frame.
                       %% This is useful for the endpoint to know what type of
                       %% fragment a potentially fragmented message is.
	 }).

%----------------------------------------------------------------------
% The state of a WebSocket connection.
% This is held by the ws owner process and passed in calls to yaws_api.
%----------------------------------------------------------------------
-type frag_type() :: text
		   | binary
		   | none.  %% The WebSocket is not expecting continuation
                            %% of any fragmented message.
-record(ws_state, {
	  vsn :: integer(),                     % WebSocket version number
	  sock,                                 % gen_tcp or gen_ssl socket
	  frag_type :: frag_type()
	 }).
