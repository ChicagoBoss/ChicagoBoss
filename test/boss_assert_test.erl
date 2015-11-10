%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors, see AUTHORS file in root directory
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     See AUTHORS file in root directory
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc
%%-------------------------------------------------------------------

-module(boss_assert_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

http_ok_test() ->

    Functions = [http_ok, http_partial_content, http_redirect,
                 http_not_modified, http_bad_request, http_not_found,
                 email_received,
                 email_not_received,email_has_text,email_has_html,
                 email_is_text_only, email_is_html_only, email_is_multipart],

    [begin
         ?assert(proper:check_spec({boss_assert, Function, 1},
                                   [{to_file, user}]))
     end|| Function <-Functions],
    ok.
