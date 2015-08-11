Using Elixir with Chicago Boss
==

Elixir is a nifty new language for the Erlang VM which you can use in parts of
your project instead of regular Erlang if you prefer.

Supported features:

* Elixir web controllers
* Ecto models
* Embedded Elixir views
* Elixir library files in src/lib/ (but no inter-file macros!)

Requirements:

* Erlang R16B or later
* A love of danger

Learn more about Elixir at http://www.elixir-lang.org/

Setup
--

To enable Elixir, you need to do the following:

1. Uncomment {elixir, ...} in the "deps" section of rebar.config in the
ChicagoBoss directory

2. Copy priv/elixir to the src/ directory in ChicagoBoss

3. Download Elixir and friends:

    ./rebar get-deps

4. First pass compile:

    ./rebar compile

5. Download Elixir dependencies:

    PATH=./deps/elixir/bin:.:$PATH mix deps.get

6. Workaround for poolboy error:

    cp rebar deps/poolboy

7. Retry Elixir dependencies:

    PATH=./deps/elixir/bin:.:$PATH mix deps.get

8. Make a new project:

    make app PROJECT=my_application


Controller API
--

Put your Elixir controllers into src/controller/ as before. The source files
should NOT be prefixed with the application name. Example:

    src/controller/puppy_controller.ex

Open up the controller and write a basic module, like:

    defmodule MyApplication.PuppyController do
        use Boss.WebController

        def index(:GET, []) do
            {:output, "Hello, world!"}
        end

        def about(:GET, []) do
            {:output, "Hello, world!"}
        end

    end

To test it out, start the server and visit:

http://localhost:8001/puppy/index

The ChicagoBoss API is essentially the same as the Erlang one.

There are two variables implicitly available to your handlers: "req" (the
SimpleBridge request object) and "session_id" (the session identifier, if
present). 

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

    def index(:GET, []) do
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


Model API
--

Ecto is database wrapper and query system written in Elixir. You can use it
instead of BossDB for a pure-Elixir data modeling experience. Read more about
Ecto here:

    https://github.com/elixir-lang/ecto

To enable Ecto in your project, set the `model_manager' config key to `ecto':

    [{boss, [
        ...
        {model_manager, ecto},
        ...
        ]
    }]

Model files go in src/model as usual. A simple model file might look like:

    defmodule MyApp.Greeting do
      use Ecto.Model

      queryable "greeting" do
        field :greeting_text, :string
      end
    end

To interact with the database, use `Boss.Repo', e.g.:

    greeting = MyApp.Greeting.new(greeting_text: "Hello, world!")

    saved_greeting = Boss.Repo.create(greeting)

    all_greetings = Boss.Repo.all(MyApp.Greeting)

You can display query results seamlessly from ErlyDTL templates. The template
engine will automatically call "get" and "to_list" on associations as needed.

Enjoy!
