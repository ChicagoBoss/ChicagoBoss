# ChicagoBoss Changelog

(Note: Prior to Version 0.8.8, changelog is contained in commit history)

### Version 0.9.0 (in development)
* Fixed bug with version in the .app file generation (@davidw)

### Version 0.8.13 - Mostly SQL Stuff... Mostly.

* Update BossDB to support `boss_db:find_by_sql/[2,3]`, allowing direct SQL
  searches to be performed if desired. Attempting this call with non-SQL based
  databases will crash. (@davidw for PostgreSQL, @choptastic for MySQL).
* Split boss_test into its own application and added as a dependency for
  boss_db.
* MySQL Test suite for BossDB now passing all tests.
* BossDB Test configuration slightly updated.
* Added `boss_db:mock_transaction/1`, which, like an actual transaction will
  lock all requests to a certain worker, without being an actual transaction.
  (used in boss_db tests).
* Fix link in rebar.config to Elixir for Erlang 17.
* Give a more useful error message when `{render_other, Location}` controller
  return is not properly formatted.
* Added sample configurations for MongoDB as well as a minimistic version of
  the boss.config. These can be found in `sample_configs/` in the main
  ChicagoBoss directory (@kotedo)
* Update Makefile to use the proper destination directory instead of just
  `../project_name` (@ingwinlu)

*(All updates by @choptastic unless specified)*

### Version 0.8.12 - Stability improvements

* Add option to make local relative redirects (@danikp)
* Fix translations with the use of 'Content-Type' headers.
* Fix update ErlyDTL to a version 0.9.4, which fixes a number of stability and
  compatibility issues.  **Update to rebar executable required for this**
* Make updates to allow compilation with Erlang 17.0
* A generated ChicagoBoss app will now default to searching "./priv" in the
  event that the directory name is different from the app name.
* Update boss_db to using 'epgsql/epgsql' (@davidw)
* Add some extra error handling where silent errors were occurring so they can
  be debugged.
* Fix routing so that matched values can appear in the action as well as the
  extra parameters.
* Some refactoring of the boss_router_controller.
* Fix Erlang node full-name and short-name incompatibilities (@davidw)
* Ensure all tests pass, removed some tests for code that was removed.
* Fix generated app issue where the 'boss' application was being loaded from
  the original location rather than from the 'deps/boss' directory as expected.
* Modify module compile-time comparison to remove unnecessary conversions to
  gregorian time.
* boss_db handle 'undefined' as a value with select queries. (@davidw)
* Consolodate ebin finding and remove duplicates (@davidw)
* Clean up inconsistent dependency versions so that `rebar update-deps` works.

*(All updates by @choptastic unless specified)*

### Version 0.8.11 - ErlyDTL Compatibility

This release have one important change since v0.8.10: we finally sorted out all
know problems with latest ErlyDTL changes.

See more info on release of ErlyDTL v0.9.1 here
https://github.com/erlydtl/erlydtl/releases/tag/0.9.1 there is a lot of cool
new stuff there

Other changes:

* fixed 'make edoc'

### Version 0.8.10 - Bugfix Release

* ErlyDTL updated, put attention to auto_escape =true by default, fro more info
  see separate release notes for ErlyDTL 0.9.0 here
  https://github.com/erlydtl/erlydtl/releases/tag/0.9.0
* A lot of improvements to testing suite (by @zkessin)
* More refactoring (by @zkessin)
* Number of great patches from our community

### Version 0.8.9

This is mostly a bugfix release with 20 issues been closed
https://github.com/ChicagoBoss/ChicagoBoss/issues?milestone=1&page=1&state=closed

Most notable:

* [NEW] JSON to model and model to JSON functionality, see api-model for
  reference (@zkessin)
* [NEW] configuration option 'ip' to bind to (#390 by @yannickg)
* [NEW] Add charset specification support to boss_mail. (#391 by @fdevibe)
* better error reporting for errors in boss.config (#246 by @zkessin)
* boss_db:type now always returns 'undefined' if no model found for provided ID
  (#249 by @zkessin)
* fixed crash when a file doesn't exit in static folder. (#389 by @mihawk)
* updated deps to latest version and moved most of them to use specific version
  instead of HEAD
* boss_db passed big refactoring

### Version 0.8.8

* Only set session if controller wants it ( @evanmiller )
* API documentation styling improved (@timClicks)
* Support HTTP/1.0 for streamed responses (@evanmiller)
* Preliminary LFE support. (@evanmiller)
* Default to Cowboy. RIP misultin (@evanmiller)
* Supporting boss apps (such as cb_admin) can be included as rebar
  dependencies. (@cstar)
* websocket improvments (@mihawk)
* support for static files in root directory. no more problems with robots.txt
  and friends, see static_files configuration option (@danikp)
* Edoc (/doc) and error pages are now have style and more helpful information
  (@danikp)
* erlydtl can use js templates (@igorclark)
* redirecting to another app fixed (@danikp)
* Support for Secure & HttpOnly session cookie flags; (@ztmr)
* Semantic versioning support; (@ztmr)
* added support for rendering templates across apps: …
  `{render_other,[{application,appname},{controller,"controller"},{action,"action"}],[]}.`
  (@ilyashuma)
* Preliminary Ecto support (@evanmiller)
* Pass a new `_req` template variable (@ztmr) Provide all the templates with a
  `_req` variable that holds the original web request so that template can
  use query and post params directly.
* support for custom boss_db cache prefix, see brigadier/boss_db@51dbe7e
  (@brigadier)
* Moving repos to CB GitHub organization (@evanmiller)
* [NEW] filters system, see README_FILTERS.md for more info (@evanmiller)
* CSRF filter for new system (@ngaranko)
* CB now passing dialyzer (@zkessin)
* better error messages, less misleading tracebacks (@zkessin)

### Versions prior to 0.8.8

Check commit history for details
