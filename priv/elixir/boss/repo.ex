defmodule Boss.Repo.Sup do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({ :local, __MODULE__ }, __MODULE__, [])
  end

  def init([]) do
    tree = [ worker(Boss.Repo, []) ]
    supervise(tree, strategy: :one_for_all)
  end
end

defmodule Boss.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def url do
    user = :erlang.list_to_binary(:boss_env.get_env(:db_username, :erlang.binary_to_list("root")))
    pass = :erlang.list_to_binary(:boss_env.get_env(:db_password, []))
    host = :erlang.list_to_binary(:boss_env.get_env(:db_host, :erlang.binary_to_list("localhost")))
    port = :boss_env.get_env(:db_port, 5432)
    name = :erlang.list_to_binary(:boss_env.get_env(:db_database, :erlang.binary_to_list("boss")))

    "ecto://#{user}:#{pass}@#{host}:#{port}/#{name}"
  end
end
