#!/bin/sh

sudo -u ttserver ttserver -host 127.0.0.1 -dmn -pid /var/run/ttserver/ttserver.pid -log /var/log/ttserver/ttserver.log /var/lib/ttserver/test.tct
