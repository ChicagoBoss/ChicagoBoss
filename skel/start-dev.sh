#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa {{src}}/ebin -pa {{src}}/deps/*/ebin -boot start_sasl -config boss -s reloader -s boss
