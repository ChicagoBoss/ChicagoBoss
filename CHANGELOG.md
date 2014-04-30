# ChicagoBoss Changelog

(Note: Prior to Version 0.8.12, changelog is contained in the tag information)

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

*(All updates by @choptastic unless specified)*
