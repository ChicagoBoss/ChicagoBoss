#!/bin/sh

# Trim the fat

rm -rf deps/*/.git
rm -rf deps/*/rebar
rm -rf deps/erlydtl/tests
rm -rf deps/mochicow/examples
rm -rf deps/epgsql/test_data
rm -rf deps/misultin/examples
rm -rf deps/boss_db/priv/test_*
rm -rf deps/cowboy/test
rm -rf deps/cowboy/examples
rm -rf deps/gen_smtp/testdata
rm -rf deps/mochiweb/support
rm -rf deps/mochiweb/examples
rm -rf deps/pmod_transform/tests
rm -rf deps/jaderl/tests
sed -i "s/{vsn, git}/{vsn, \"0\"}/" deps/*/src/*.app.src
