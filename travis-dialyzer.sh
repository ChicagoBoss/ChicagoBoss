#!/bin/bash


PLT=plt/cb-$RANDOM.plt
echo "PLT File $PLT"
export PATH=$PATH:/usr/local/bin:/usr/bin
echo "Building PLT, may take a few minutes"
dialyzer  --build_plt --apps kernel stdlib\
       --output_plt $PLT > /dev/null
for app in  mnesia  inets ssl crypto \
       erts public_key runtime_tools compiler asn1 hipe\
       syntax_tools 
do 
    echo $app
    dialyzer --add_to_plt --apps $app\
       --plt $PLT > /dev/null
done
rm -f deps/riak_*/ebin/*_pb.beam  
echo "********************************************************************************"
for app in $(ls deps/)
do
   echo "Adding $app"
   dialyzer --add_to_plt --apps deps/$app \
       --plt $PLT > /dev/null


done
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



