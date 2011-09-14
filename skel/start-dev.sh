#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin \
     -pa deps/*/ebin \
     -boss developing_app {{appid}} \
     -boot start_sasl -config boss -s reloader -s boss \
     -sname wildbill
