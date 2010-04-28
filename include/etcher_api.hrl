%%%-------------------------------------------------------------------
%%% File    : api.hrl
%%% Project : Etcher (http://github.com/jinsky/etcher)
%%% Author  : Rory Byrne <rory [at] jinsky [dot] com>
%%% License : BSD
%%%
%%% Copyright (c) 2010 Rory Byrne
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   * Redistributions of source code must retain the above copyright
%%%     notice, this list of conditions and the following disclaimer.
%%%
%%%   * Redistributions in binary form must reproduce the above
%%%     copyright notice, this list of conditions and the following
%%%     disclaimer in the documentation and/or other materials provided
%%%     with the distribution.
%%%
%%%   * Neither the names of the copyright holders, nor the names of its
%%%     contributors may be used to endorse or promote products derived
%%%     from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-record(filter_def, {
            name,                   % string() - eg "truncatewords"
            mf,                     % {Mod, Fun}
            arg_required            % true | false | optional
            }).

-record(tag_def, {
            name,                   % string() - eg "autoescape"
            mf,                     % {Mod, Fun}
            must_be_first = false   % Some tags, such as 'extends' are 
                                    % required to be the first non-text
                                    % entity in a template (they can be 
                                    % preceeded by a text prequel, but not
                                    % by variables or other tags).
            }).

-record(string, {
            val,
            safe = false
            }).

-define(EMPTY_STRING, #string{val=""}).

-record(ref, {parts}).

-record(variable, { 
            val, 
            filters                     % [#filter{}]
            }).

-record(filter, {
            name,                       % string()
            arg,
            mf
            }).

-record(tag, {
            name,                       % string()
            extra = ""                  % string()
            }).

% Parser State
-record(ps, {
            tokens, 
            tag_db,
            filter_db,
            template_loaders = [],      % [{Mod, Fun, Args}]
            compile_trail = []          % For loop avoidance. Only needed
                                        % by tags which can do compile-time
                                        % includes (eg, the 'include' tag).
                                        % Most tags which include other 
                                        % templates do so at render-time.
                                        % See also #rs.render_trail
            }).

% Renderer State
-record(rs, {
            context = [],
            globals = [],
            auto_escape = true,
            template_string_if_invalid = ?EMPTY_STRING,
            template_loaders = [],          % [{Mod, Fun, Args}]
            render_trail = [],              % For loop avoidance
            compiler_opts = [],             % Compiler options for when
                                            % the renderer is dynamically
                                            % compiling included templates.
                                            % Eg, for 'extends', 'include'
                                            % and 'ssi' tags.
            allowed_include_roots = [],     % for ssi tag
            url_mapper                      % optional - user submitted
            }).

-define(MAJOR_TVER, 1).
-define(MINOR_TVER, 0).
-define(CURRENT_TVER, {?MAJOR_TVER, ?MINOR_TVER}).

% Template Record
-record(etcher_template, {
            version = ?CURRENT_TVER,   % Template Version as {Major, Minor}

            md5,                       % MD5 of UTF-8 encoded template source.
                                       %
                                       % Note that Etcher does not use this as 
                                       % a checksum field - it's main purpose 
                                       % is for loop detection. Two templates
                                       % with the same content will have the 
                                       % the same id.
                                       % 
                                       % Also, *you* should not use this as a 
                                       % checksum field. Roll your own in 
                                       % the opaque field.

            created,                   % Timestamp. Same format as returned 
                                       % by erlang:now():
                                       % {MegaSecs, Secs, MicroSecs}

            content,                   % Core Template Content - list()

            sig,                       % Placeholder for a digital signature. 
                                       % Etcher ignores this field.

            opaque                     % Another placeholder. Allows you to store 
                                       % whatever information you want with a 
                                       % particular template (eg: author name).
                                       % Etcher ignores this field 
           }).

