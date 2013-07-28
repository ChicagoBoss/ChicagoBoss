defmodule Boss.Flash do
  @moduledoc """
  Storage for temporary ("Flash") messages associated with sessions.
  """

  @spec get_and_clear(:string) :: [any]
  @doc "Retrieve and remove all messages for the given session_id"
  def get_and_clear(session_id) do
    :boss_flash.get_and_clear(:erlang.binary_to_list(session_id))
  end

  @spec add(:string, :atom, any) :: :ok | {:error, any}
  @doc "Add a message to the flash message stack for session_id"
  def add(session_id, type, title) do
    :boss_flash.add(:erlang.binary_to_list(session_id))
  end

  @spec add(:string, :atom, any, any) :: :ok | {:error, any}
  @doc "Add a message to the flash message stack for session_id"
  def add(session_id, type, title, message) do
    :boss_flash.add(:erlang.binary_to_list(session_id), type, title, message)
  end
end
