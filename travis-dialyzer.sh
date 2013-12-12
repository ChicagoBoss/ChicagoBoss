#!/bin/bash


PLT=plt/cb.plt
echo "PLT File $PLT"
export PATH=$PATH:/usr/local/bin:/usr/bin
echo "Building PLT, may take a few minutes"
dialyzer  --build_plt --apps kernel stdlib mnesia  inets ssl crypto \
       erts public_key runtime_tools compiler asn1 hipe gs\
       syntax_tools edoc xmerl \
       --statistics\
       --output_plt $PLT
rm -f deps/riak_*/ebin/*_pb.beam  
echo "********************************************************************************"
dialyzer --add_to_plt deps/*/ebin						--plt $PLT
echo "********************************************************************************"
echo ""

dialyzer	ebin/		\
    -Werror_handling		\
    -Wno_undefined_callbacks	\
    -Wrace_conditions		\
    --statistics -n             \
    --fullpath			\
    -n                          \
    --plt $PLT #  -Wunmatched_returns 
#  



