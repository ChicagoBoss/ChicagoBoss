#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa {{src}}/ebin -boot start_sasl -config boss -s reloader -s boss
