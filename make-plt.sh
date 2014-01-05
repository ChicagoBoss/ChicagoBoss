#!/bin/bash
clear

PLT=plt/cb.plt
echo $PLT

if [ ! -f $PLT ]; then
   dialyzer  --build_plt --apps kernel stdlib mnesia  inets ssl crypto \
       erts public_key runtime_tools compiler asn1 hipe gs\
       syntax_tools edoc xmerl public_key inets \
       --statistics\
       --output_plt $PLT
   rm deps/riak_core/ebin/*.beam
   echo "********************************************************************************"
   dialyzer --add_to_plt deps/*/ebin						--plt $PLT
   echo "********************************************************************************"
   echo ""
fi
