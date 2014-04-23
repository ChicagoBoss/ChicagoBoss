Upgrade: From 0.8.x to 0.8.12 (where x < 12)

You need a new version of rebar otherwise you are very likely to see weird
errors compiling templates. The easiest way is probably with:

    wget https://github.com/ChicagoBoss/ChicagoBoss/raw/master/skel/rebar
    chmod a+x rebar

Upgrade: From 0.7 to 0.8
------------------------

Calls to boss_db:find/3-6 need to be replaced with the new boss_db:find/3 API.
The third argument should a proplist with options for 'limit', 'offset',
'order_by', and 'descending'.

The 'sort_by' option to the -has BossRecord attribute has been renamed
'order_by'.


Upgrade: From anything to 0.7.2
-------------------------------

Custom tags should be moved from view/lib/ to view/lib/tag_html.


Upgrade: From 0.6 to 0.7
------------------------

In Chicago Boss 0.7, projects are now rebarified, boss.config is the central
place for all configuration and eunit testing has been made easy,

bc. ./rebar boss # (list of available commands)
./init.sh # (list of available boot commands)

h3. Migrate current cb apps

In order to ease the task, a new make task has been created in the framework
Makefile, just update your Chicago Boss and run:

bc. make rebarize APPDIR=/path/to/app

This will copy all new files in your app directory, move the app.src file to
src, move the boss_web_tests (functionals) to src/test/functional and point you
with instructions on what you need add to your boss.config.

bc. make rebarize APPDIR=../my_app
../my_app rebar-boss-ified
WARNING: your boss.config have not been changed, you need to set:
- in boss app section:
---- {path, "/path/to/chicago_boss"}
---- {vm_cookie, "my_secret_cookie"} % Optional, defaults to abc123
- for each app defined:
---- {path, "../path/to/app"}
INFO: you can safely remove the Makefile and start* files from your app dir
INFO: after the boss.config change, you can run:
cd ../my_app
./rebar boss # Shows all boss-rebar commands
./init.sh # Shows the new boot system commands

h3. Compilation

Boss has non-standard-compatible-rebar-way of compiling code, the rebar plugin
switches off standard compilation and calls the boss_load module.

./rebar compile (works for compatibility mode)
./rebar boss c=compile

h3. Configuration

Now the only file on a cb project you need to take care is boss.config, the new
init.sh calls ./rebar boss commands that returns automatically generated erl
-pa... commands and executes it.

You now defines the path in boss.config for each app and (compilation, test
launch, start, stop, reload, ...) works automatically.

A new setting in boss.config called vm_cookie (optional) has been added, used
in start/stop/reload commands (defaults to "abc123"):

bc. [{boss, [ {path, "../ChicagoBoss"}, {vm_cookie, "abc123"}, ...

h3. Tests

Eunit testing compilation/setup works automatically, start writing unit tests,
just place them in "src/test/eunit" and run ./rebar eunit or ./rebar boss
c=test_eunit

Functional tests lives now in src/test/functional (before was placed in
src/test), just call ./rebar boss c=test_functional

h3. Init

The init.sh handles start, start-dev as before, but also stop, reload (hot for
the node) and restart is implemented.

Now we can tweak, change, make vm.args configurable (in boss.config) without
force developers to handle this manually in their apps.

Upgrade: From 0.5 to 0.6
------------------------

Starting with Chicago Boss 0.6, projects follow an OTP layout, and the admin
interface is a standalone OTP application.

To upgrade, you will need to do the following in your project:

    mkdir src
    mkdir priv
    mv controller/ init/ lib/ mail/ model/ test/ view/ src/
    mv lang/ static/ priv/
    PROJECT=my_application mv boss.routes priv/$PROJECT.routes

In addition, you will need to prefix your controller modules with the
application name. E.g.

    mv blog_controller.erl my_application_blog_controller.erl

This avoids module naming conflicts when multiple applications are installed on
the same server (which was not possible prior to 0.6).


Upgrade: From 0.5.x to 0.5.y
----------------------------

Starting with Chicago Boss 0.5, the framework source is kept separate from your
project source. After downloading and building a new copy of CB, you need to
update your server scripts to point to the new source. Open "start-dev.sh" and
"start.sh" and make sure there is an argument like this:

    -pa /path/to/new/ChicagoBoss/ebin

You might also want to update your /admin interface by copying the "skel/admin"
directory from the new CB over to your project directory.


Upgrade: From 0.4 to 0.5
------------------------

The easiest way to upgrade from 0.4 is to create a new CB app and copy your
source into it. In the CB source directory, type:

    make
    make app PROJECT=mynewproject
    cd ../mynewproject
    cp -a /path/to/existing/project/view \
          /path/to/existing/project/model \
          /path/to/existing/project/controller \
          /path/to/existing/project/static \
          /path/to/existing/project/boss.config \
          .

Then start the server with ./start-dev.sh


Upgrade: From 0.4.x to 0.4.y
----------------------------

Starting with Chicago Boss 0.4, application upgrades are as simple as:

   cp -a /path/to/old/ChicagoBoss/[a-z]* .

In general, files in the project directory that start with an upper-case letter
belong to Boss. Files which start with a lower-case letter belong to you.


Upgrade: From 0.3 to 0.4
------------------------

The directory structure has changed somewhat. The MVC files have moved around,
and files for the /admin interface are in a separate directory (ADMIN). 

The following steps should get you most of the way to an upgrade:

   cp -a /path/to/old/ChicagoBoss/boss.config .
   cp -a /path/to/old/ChicagoBoss/Web/*_controller.erl controller/
   rm controller/admin_controller.erl
   cp -a /path/to/old/ChicagoBoss/Web/*.erl lib/
   cp -a /path/to/old/ChicagoBoss/Model/* model/
   cp -a /path/to/old/ChicagoBoss/Web/*_views view/
   rm -r view/admin_views
   cp -a /path/to/old/ChicagoBoss/static/* static/
   make clean
   make
