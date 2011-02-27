Getting Started With Chicago Boss
=================================

Quickstart
----------

Dependencies:

* Erlang R13A or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.

Build Chicago Boss with `make`.

* On Windows Vista or Windows 7 -

    1. you need install win openSSl (http://www.slproweb.com/products/Win32OpenSSL.html)
    2. make mochiweb with msys or cygwin

When you're ready to start the server, run `./START-DEV.SH` (or start-dev.bat)
in this directory (admin privileges needed).  There will be a lot of PROGRESS
REPORTs which look scary but hopefully everything is running smoothly. With
this console you can interact directly with the running server. Next, point
your browser to:

    http://localhost:8001/

If all is well you will see "Hello, World!" Now you can get busy. 


Documentation
-------------

See the FAQ and API files located at

<http://www.chicagoboss.org/>

If you need help getting started, check out "An Evening With Chicago Boss":

<http://www.evanmiller.org/chicago-boss-guide.html>

There's also the mailing list:

<http://groups.google.com/group/chicagoboss>
