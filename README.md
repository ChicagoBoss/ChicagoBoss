Getting Started With Chicago Boss
=================================

Quickstart
----------

    make
    make app PROJECT=mynewproject
    cd ../mynewproject
    ./start-dev.sh

Then visit http://localhost:8001/ in your browser.

There will be a lot of PROGRESS REPORTs but everything should be running smoothly.


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

See README_UPGRADE


Database Setup
--------------

See README_DATABASE


Developer Documentation
-----------------------

See the FAQ and API files located at

<http://www.chicagoboss.org/>

If you need help getting started, check out "An Evening With Chicago Boss":

<http://www.evanmiller.org/chicago-boss-guide.html>

There's also the mailing list:

<http://groups.google.com/group/chicagoboss>
