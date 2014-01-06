#!/bin/bash

export PATH=$PATH:/usr/local/bin:/usr/bin
PLT=plt/cb.plt

echo ""
dialyzer	ebin/		\
    -Werror_handling		\
    -Wno_undefined_callbacks	\
    -Wrace_conditions		\
    --fullpath			\
    --plt $PLT #  -Wunmatched_returns -n
#  



