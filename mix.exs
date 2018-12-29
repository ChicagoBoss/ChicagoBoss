defmodule Boss.Mixfile do
  use Mix.Project

  def project do
    [ app: :boss,
      version: "0.9.0-beta2",
      deps: deps(Mix.env),
      name: "Chicago Boss",
      source_url: "https://github.com/ChicagoBoss/ChicagoBoss",
      elixir: "~> 0.10.3" ]
  end

  def application do
    []
  end

  defp deps(_) do
    [ { :ecto, github: "elixir-lang/ecto" },
    { :postgrex, github: "ericmj/postgrex" } ]
  end
end
