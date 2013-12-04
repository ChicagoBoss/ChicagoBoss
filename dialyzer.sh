#!/bin/bash

export PATH=$PATH:/usr/local/bin:/usr/bin
PLT=plt/cb.plt

if [ ! -f $PLT ]; then
   dialyzer  --build_plt --apps kernel stdlib mnesia  inets ssl crypto \
       --output_plt $PLT

   dialyzer  --add_to_plt deps/*/src  --output_plt $PLT
   # for app in "${apps[@]}"; do
   #     echo "Adding $app to PLT\n"
   #     dialyzer  --add_to_plt $app  --output_plt $PLT
   ##done
   echo "********************************************************************************"
   echo ""
fi
rebar compile skip_deps=true||exit
#rm ebin/*template.beam

echo ""
dialyzer	ebin/		\
    -Werror_handling		\
    -Wno_undefined_callbacks	\
    -Wrace_conditions		\
    --fullpath			\
    -n \
    --plt $PLT #  -Wunmatched_returns -n
#  



