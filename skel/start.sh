#!/bin/sh
cd `dirname $0`
# For multi-node setups you need to change the -sname argument for every node
# and also set the master_node config option. The master node runs global
# services (sessions, message queue, events, and incoming mail) and should
# be specified as the atom NodeName@NodeHost

# If your server is running in an untrusted environment, you should probably
# change the cookie too. (All nodes in a cluster must have the same cookie.)
exec erl +K true -pa $PWD/ebin -pa {{src}}/ebin -pa {{src}}/deps/*/ebin \
    -boot start_sasl -config boss -s boss -setcookie abc123 -detached \
    -sname john 
