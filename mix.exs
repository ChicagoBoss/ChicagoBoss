defmodule Boss.Mixfile do
  use Mix.Project

  def project do
    [ app: :boss,
      version: "0.8.7",
      deps: deps(Mix.env),
      name: "Chicago Boss",
      source_url: "https://github.com/evanmiller/ChicagoBoss",
      elixir: "~> 0.10.3" ]
  end

  def application do
    []
  end

  defp deps(_) do
    [ { :ecto, github: "elixir-lang/ecto" },
    { :pgsql, github: "ericmj/pgsql", branch: "elixir" } ]
  end
end
