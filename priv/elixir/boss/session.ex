defmodule Boss.Session do
  @moduledoc """
  Get and set values in Boss's session storage.
  """

  @doc """
  Retrieve all keys and values in the session storage associated with a given
  session ID. Returns a proplist
  """
  @spec get_session_data(:string) :: [{any, any}]
  def get_session_data(session_id) do
    :boss_session.get_session_data(:erlang.binary_to_list(session_id))
  end

  @doc """
  Retrieve the value for a given key associated with a given session ID
  """
  @spec get_session_data(:string, :term) :: any
  def get_session_data(session_id, key) do 
    :boss_session.get_session_data(:erlang.binary_to_list(session_id), key)
  end

  @doc """
  Set the value for a key associated with a given session ID
  """
  @spec set_session_data(:string, :term, :term) :: :ok | {:error, any}
  def set_session_data(session_id, key, value) do
    :boss_session.set_session_data(:erlang.binary_to_list(session_id), key, value)
  end

  @doc """
  Remove any value for the given key and session ID
  """
  @spec remove_session_data(:string, :term) :: :ok | {:error, any}
  def remove_session_data(session_id, key) do
    :boss_session.remove_session_data(:erlang.binary_to_list(session_id), key)
  end
end
