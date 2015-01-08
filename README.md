Chicago Boss: Start small, dream big
====================================
[![Build Status](https://travis-ci.org/ChicagoBoss/ChicagoBoss.svg?branch=master)](https://travis-ci.org/ChicagoBoss/ChicagoBoss)

Chicago Boss is a server framework inspired by Rails and written in Erlang. It
offers all the conveniences of modern web development, including Comet. What sets
Chicago Boss apart from other non-Erlang frameworks is that it can handle large
amounts of traffic without any drop in performance. What sets Chicago Boss apart
from other Erlang frameworks is that it is easy to set up and use.

WARNING: Chicago Boss does not work with Erlang R16B03 due to an error in erl_syntax

60-second Quickstart
--------------------

After downloading and extracting, type

```console
make
make app PROJECT=mynewproject
cd ../mynewproject
./init-dev.sh
```

For Windows, type

```console
windows-make.bat
windows-make.bat app PROJECT=mynewproject
cd ..\mynewproject
start-server.bat
```

Then visit http://localhost:8001/ in your browser. Congratulations, you have
a web server. There will be a lot of PROGRESS REPORTs on your console but
everything should be running smoothly.

The project name should be a legal Erlang atom, i.e. start with a lowercase
letter and contain only letters, digits, and underscores (for easy compatibility is recommended name the project dir and app name the same).


Dependencies
------------

* Erlang R16B or later -

    <http://www.erlang.org/download.html>

  * Check with `erlang:system_info(otp_release)`.


* On Windows Vista or Windows 7 -

  * Erlang bin directory must be in PATH.


Admin Interface
---------------

You probably want to install the CB admin interface. Download it from

    <https://github.com/ChicagoBoss/cb_admin>


Upgrades
--------

Uprading used to be a pain but now it is easy. See README_UPGRADE


Database Setup
--------------

By default CB uses an in-memory database that needs no configuration. To start
working with an actual database, See README_DATABASE


Philosophy and Features
-----------------------

Why another web framework? Because Rails apps are slow and Node apps are messy.
Chicago Boss takes advantage of functional programming and under-the-hood
compiler magic to provide clean, understandable controller logic, Django-style
templates, and an ORM based on Erlang's parameterized modules. The best part is
that the network I/O is 100% asynchronous so you can seamlessly integrate Comet
endpoints into your app, and you never have to worry about a slow database
query dragging down unrelated requests.

CB ships with all the tools you need to build a feature-ful website, including
sessions, URL routing, filtering requests and post-processing responses,
frameworks for sending and receiving email, JSON generation, Comet via
long-poll and message queues, and internationalization (i18n). Read on for
details.

*Databases*. Chicago Boss currently supports MySQL, PostgreSQL, Tokyo Tyrant,
Mnesia, MongoDB, and Riak. In CB 0.5.4 and later, you can mix and match
databases by configuring Boss to use vertical shards. For SQL databases, the
conventions are similar to Rails (plural nouns for the table names, object_id
for foreign keys, etc.).

*BossRecords*. Boss's take on ActiveRecord is called a BossRecord, which is
an Erlang parameterized module on steroids. You instantiate a BossRecord like
a regular parameterized module:

```erlang
Article = article:new('id', "This is a title", "This is a body")
```

But then CB generates functions and attaches them to BossRecords, so you can
write code like

```erlang
{ok, SavedArticle} = Article:save()
```

Before saving to the database, the save() function will call a function called
validation_tests(), where you can perform custom validation logic.

CB also generates getter functions which can be invoked directly in templates,
so in your template you can write things like

```erlang
{{ article.title }}
```

Speaking of which...

*Templates*. Chicago Boss uses ErlyDTL, an Erlang implentation of Django template
language. In fact, Chicago Boss originated with a rewrite of ErlyDTL, and the same
person maintains both projects so you always get the latest ErlyDTL features. Templates
can access and loop over values stored in proplists, dictionaries, and BossRecords,
so you can move data from the database to your templates with a minimum of massaging.

In addition, templates are tightly integrated with Boss's i18n machinery. The admin
interface automatically parses templates for translatable strings, and Boss can
choose which language to serve based on the request's Accept-Languages header and
the calculated translation coverage for a particular page. Multi-language websites
are easy to develop with Chicago Boss.

*Controllers*. Erlang's pattern-matching is a perfect fit for writing
controller logic. Your controllers are passed the URL method (GET, POST) and a list
of URL tokens, and just need to return a tuple telling Boss what to do, for example:

* `{ok, Variables}` - render the default template with Variables
* `{render_other, Variables}` - render another template
* `{redirect, URL}` - redirect the request
* `{json, Proplist}` - encode the Proplist as JSON and send to the client
* `{output, Data}` - send Data to the client

If you come from Rails, you'll instantly notice the benefit of Erlang's
language design: you don't need an ugly `case request.method` statement inside
every action, you never have atrocities like `render and return`, and you can
always see every variable that is in scope. In CB apps, controller logic is
always concise and usually a pleasure to read.

*Sessions*. You can configure sessions to be stored in memory (ETS) or in an
Mnesia database.  The `boss_session` and `boss_flash` modules provide functions
for storing and retrieving session information.

*Routes*. By default, Chicago Boss uses the same routing conventions as Ruby on
Rails (`/controller/action/id`). You can customize the routes and provide
a base URL in the priv/application.routes file. Of course, most routing occurs
with the pattern-matching controller logic, e.g.
```erlang
posts('GET', ["category", Category]) ->
```

You can then generate URLs to match controller patterns in your templates like
so:

    {% url action="posts" category="some category" %}

*Email*. Chicago Boss ships with a miniature MVC for sending multipart emails.
Emails can be templated with ErlyDTL, and it is easy to provide plain-text and
HTML versions of the same email. In testing environments, email can be sent
directly to the recipient, or in production can be relayed to an SMTP server.

A Chicago Boss server can also receive email over SMTP. Each email address maps
to a function in your incoming mail controller, so it is easy to write
well-organized email applications. Your email controller has access to the
exact same resources as your web controllers, including database requests,
BossRecord instantiation, and the message queue; web and email are fully
integrated.

*Comet*. It's simple to write a Comet endpoint in Chicago Boss. Unlike any
other language, Erlang gives you the benefits of asynchronous network
communcation without using callbacks. Here is a trivial example of a long-poll
controller:

```erlang
longpoll('GET', [Channel]) ->
    {ok, Timestamp, Messages} = boss_mq:pull(Channel, last),
    {json, [{timestamp, Timestamp}, {messages, Messages}]}.
```

The call to `pull` blocks until a message is received. Because processes are
cheap in Erlang, the overhead of keeping alive a blocking request is very small
(just a few kilobytes of memory, compared to megabytes in Rails). You can
thus keep alive thousands of Comet request with just a few megabytes of memory.
Also notice that the controller logic remains nice and clean (no callbacks). We
can perform an arbitrary sequence of asynchronous network requests without
increasing the scope level.

*Events*. An interesting feature of Chicago Boss is the events API called
BossNews. With it, you can watch a record or a set of records for changes,
then execute a callback when a change is witnessed. Combined with long-polling,
you can provide a real-time view of your database on your website. Events can
also be used to provide a clean separation of business logic and notification
logic.

*Tests*. Chicago Boss has a kick-ass testing framework. Once you try it you
won't go back. That's all I'll say for now.


Future Work
-----------

Most of Chicago Boss's planned features have been implemented at this point. It
hasn't really been tested in a distributed environment, but it's already
running on a few public-facing websites, and many more internal websites (or so
I am told). It would be nice to add more databases (such as CouchDB and Oracle)
and support horizontal sharding. The last main feature before 1.0 will be
the ability to distribute apps written in Chicago Boss as standalone OTP
applications.


Further Reading
---------------

See the FAQ and API files located at

<http://www.chicagoboss.org/>

If you need help getting started, check the new pdf tutorial:

<http://www.chicagoboss.org/tutorial.pdf>

Be sure to also check the wiki

<https://github.com/ChicagoBoss/ChicagoBoss/wiki>

There's also the mailing list:

<http://groups.google.com/group/chicagoboss>

If you want to contribute to CB

[CODING_STANDARDS.md](https://github.com/ChicagoBoss/ChicagoBoss/blob/master/CODING_STANDARDS.md)

View the CHANGELOG.md

[CHANGELOG.md](https://github.com/ChicagoBoss/ChicagoBoss/blob/master/CHANGELOG.md)
