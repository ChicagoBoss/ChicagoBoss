defmodule Boss.WebController do

  defmacro __using__(_) do
    quote do
      Module.register_attribute __MODULE__, :__routes__, persist: false, accumulate: true
      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  defmacro get(action, tokens, block) do
    handle(:GET, action, tokens, block)
  end

  defmacro get(action, tokens, info, block) do
    handle(:GET, action, tokens, block, info)
  end

  defmacro post(action, tokens, block) do
    handle(:POST, action, tokens, block)
  end

  defmacro post(action, tokens, info, block) do
    handle(:POST, action, tokens, block, info)
  end

  defmacro put(action, tokens, block) do
    handle(:PUT, action, tokens, block)
  end

  defmacro put(action, tokens, info, block) do
    handle(:PUT, action, tokens, block, info)
  end

  defmacro delete(action, tokens, block) do
    handle(:DELETE, action, tokens, block)
  end

  defmacro delete(action, tokens, info, block) do
    handle(:DELETE, action, tokens, block, info)
  end

  defmacro before_(action, block) do
    quote do
      def before_(var!(req), var!(session_id), unquote(action)), unquote(block)
    end
  end

  defmacro before_(action, method, tokens, block) do
    quote do
      def before_(var!(req), var!(session_id), unquote(action), unquote(method), unquote(tokens)), unquote(block)
    end
  end

  defmacro cache_(action, tokens, block) do
    quote do
      def cache_(var!(req), var!(session_id), unquote(action), unquote(tokens)), unquote(block)
    end
  end

  defmacro cache_(action, tokens, info, block) do
    quote do
      def cache_(var!(req), var!(session_id), unquote(action), unquote(tokens), unquote(info)), unquote(block)
    end
  end

  defmacro lang_(action, block) do
    quote do
      def lang_(var!(req), var!(session_id), unquote(action)), unquote(block)
    end
  end

  defmacro lang_(action, info, block) do
    quote do
      def lang_(var!(req), var!(session_id), unquote(action), unquote(info)), unquote(block)
    end
  end

  defmacro after_(action, result, block) do
    quote do
      def after_(var!(req), var!(session_id), unquote(action), unquote(result)), unquote(block)
    end
  end

  defmacro after_(action, result, info, block) do
    quote do
      def after_(var!(req), var!(session_id), unquote(action), unquote(result), unquote(info)), unquote(block)
    end
  end

  defmacro req() do
    quote do
      :erlang.get("BOSS_INTERNAL_REQUEST_OBJECT")
    end
  end

  defmacro session_id() do
    quote do
      :erlang.get("BOSS_INTERNAL_SESSION_ID")
    end
  end

  defp handle(method, action, tokens, block) do
    action = to_action(action)
    route = {action, to_route_tokens(tokens)}
    quote do
      @__routes__ unquote(route)
      def unquote(action)(var!(req), var!(session_id), unquote(method), unquote(tokens)), unquote(block)
    end
  end

  defp handle(method, action, tokens, block, info) do
    action = to_action(action)
    route = {action, to_route_tokens(tokens)}
    quote do
      @__routes__ unquote(route)
      def unquote(action)(var!(req), var!(session_id), unquote(method), unquote(tokens), unquote(info)), unquote(block)
    end
  end

  defp to_action({atom, _, _}), do: atom
  defp to_action(action),       do: action

  defp to_route_tokens({_, _, nil}), do: []
  defp to_route_tokens(tokens) do
    lc token inlist tokens, do: to_route_token(token)
  end

  defp to_route_token(token) do
    case token do
      {:|, _, [token, _]} -> to_route_token(token)
      {name, _, _}        -> name
      value               -> value
    end
  end

  defmacro __before_compile__(_) do
    quote do
      def _routes(_),    do: @__routes__
      # simulate a new && instance functions
      def new(req), do: {__MODULE__, req}
      def instance(req), do: {__MODULE__, req}
    end
  end

end
