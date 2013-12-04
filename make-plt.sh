#!/bin/bash
clear

PLT=plt/api.plt
echo $PLT

if [ ! -f $PLT ]; then
   dialyzer  --build_plt --apps kernel stdlib mnesia  inets ssl crypto \
       erts public_key runtime_tools compiler asn1 hipe gs\
       syntax_tools edoc xmerl \
       --statistics\
       --output_plt $PLT
   rm deps/riak_core/ebin/riak_core_pb.beam
   echo "********************************************************************************"
   dialyzer --add_to_plt deps/e2/ebin						--plt $PLT
   dialyzer --add_to_plt deps/chi2/ebin						--plt $PLT
   dialyzer --add_to_plt deps/edate/ebin deps/emysql/ebin			--plt $PLT
   dialyzer --add_to_plt deps/erlcloud/ebin deps/erlydtl/ebin deps/jsx/ebin	--plt $PLT
   dialyzer --add_to_plt deps/lager/ebin deps/oauth/ebin deps/restc/ebin	--plt $PLT
   dialyzer --add_to_plt deps/time_interval/ebin deps/vclock/ebin		--plt $PLT
   dialyzer --add_to_plt deps/webmachine/ebin					--plt $PLT
   dialyzer --add_to_plt deps/jsxd/ebin deps/uuid/ebin deps/erlando/ebin	--plt $PLT
   dialyzer --add_to_plt deps/mochiweb/ebin deps/mochiweb_util/ebin		--plt $PLT
   dialyzer --add_to_plt deps/riak_core/ebin					--plt $PLT
   echo "********************************************************************************"
   echo ""
fi
