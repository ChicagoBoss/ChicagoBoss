Chicago Boss: Build Something Big
=================================

Chicago Boss is a server framework inspired by Rails and written in Erlang. It
offers all the conveniences of modern web development, including Comet. What sets
Chicago Boss apart from other non-Erlang frameworks is that it can handle large
amounts of traffic without any drop in performance. What sets Chicago Boss apart
from other Erlang frameworks is that it doesn't suck.


60-second Quickstart
--------------------

After downloading and extracting, type

    make
    make app PROJECT=mynewproject
    cd ../mynewproject
    ./start-dev.sh

Then visit http://localhost:8001/ in your browser. Congratulations, you have
a web server. There will be a lot of PROGRESS REPORTs on your console but
everything should be running smoothly.


Dependencies
------------

* Erlang R13A or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.


* On Windows Vista or Windows 7 -

    1. you need install win openSSl (http://www.slproweb.com/products/Win32OpenSSL.html)
    2. make mochiweb with msys or cygwin


Upgrades
--------

Uprading used to be a pain but now it is easy. See README_UPGRADE


Database Setup
--------------

By default CB uses an in-memory database that needs no configuration. To start
working with an actual database, See README_DATABASE


Developer Documentation
-----------------------

See the FAQ and API files located at

<http://www.chicagoboss.org/>

If you need help getting started, check out "An Evening With Chicago Boss":

<http://www.evanmiller.org/chicago-boss-guide.html>

There's also the mailing list:

<http://groups.google.com/group/chicagoboss>
