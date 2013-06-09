Using Elxir with Chicago Boss
==

Elixir is a nifty new language for the Erlang VM which you can use in parts of
your project instead of regular Erlang if you prefer.

Supported features:

* Elixir web controllers
* Embedded Elixir views
* Elixir library files in src/lib/ (but no inter-file macros!)

Requirements:

* Erlang R15B01 or later
* A love of danger

Learn more about Elixir at http://www.elixir-lang.org/

Setup
--

To enable Elixir, you need to do the following:

1. Uncomment {elixir, ...} in the "deps" section of rebar.config in the
ChicagoBoss directory

2. Copy priv/web_controller.ex to the src/ directory in ChicagoBoss

3. Download Elixir:

    ./rebar get-deps

4. Compile everything:

    ./rebar compile

5. Make a new project:

    make app PROJECT=my_application


Controller API
--

Put your Elixir controllers into src/controller/ as before. The source files
should NOT be prefixed with the application name. Example:

    src/controller/puppy_controller.ex

Open up the controller and write a basic module, like:

    defmodule MyApplication.PuppyController do
        use Boss.WebController

        get :index, [] do
            {:output, "Hello, world!"}
        end

        get about, [] do
            {:output, "Hello, world!"}
        end

    end

To test it out, start the server and visit:

http://localhost:8001/puppy/index

The ChicagoBoss API is essentially the same as the Erlang one. Note that
functions corresponding to the same action should be grouped together:

    get :index, [] do {:output, "Good"} end
    get :index2, [] do {:output, "Still good"} end
    post :index2, [] do {:output, "Fine"} end
    post :index, [] do {:output, "BAD"} end

There are two variables implicitly available to your handlers: "req" (the
SimpleBridge request object) and "session_id" (the session identifier, if
present). If you don't like implicit variables, you can write your handlers
in an explicit style:

    def index(req, session_id, :GET, []) do
        {:output, "Hello, world!"}
    end

Other macros available include "before_", "cache_", "after_", and "lang_",
which take the same arguments as their Erlang counterparts.

Tokens are passed in as Elixir strings (binaries), not Erlang strings.

Feel free to mix and match Elixir and Erlang controllers in the same project,
but watch out for name collisions.

You can access the regular Chicago Boss Erlang API using atoms:

   get :index, [] do
      puppies = :boss_db.find(:puppy, [{:name, :equals, "Fido"}])
      {:ok, [{:puppies, puppies}]}
   end


View API
--

Chicago Boss also support EEx files (Embedded Elixir). These should go in
src/view/<controller name>/ and have an extension ".eex". Example:

    src/view/puppy/index.eex

You can open it up and write a basic template like:

    Puppy name: <%= @puppy.name %>

Note that top-level variables should be prefixed with @.

To populate this template, you'll want something in your controller like:

    get :index, [] do
        {:ok, [{:puppy, [{:name, "Fido"}]}]}
    end

To write a loop, use Enum.map:

    <%= Enum.map @puppies, fn(pup) -> %>
        Pupply name: <%= pup.name %><br>
    <% end %>

This is a comment:

    <%# Comment %>

Of course, you're free to continue using Django templates with extension ".dtl"
if you prefer a dedicated template language.

Enjoy!
