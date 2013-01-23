defmodule Boss.WebController do
  defmacro get(action, tokens, block) do
    handle(:get, action, tokens, block)
  end

  defmacro get(action, tokens, info, block) do
    handle(:get, action, tokens, block, info)
  end

  defmacro post(action, tokens, block) do
    handle(:post, action, tokens, block)
  end

  defmacro post(action, tokens, info, block) do
    handle(:post, action, tokens, block, info)
  end

  defmacro put(action, tokens, block) do
    handle(:put, action, tokens, block)
  end

  defmacro put(action, tokens, info, block) do
    handle(:put, action, tokens, block, info)
  end

  defmacro delete(action, tokens, block) do
    handle(:delete, action, tokens, block)
  end

  defmacro delete(action, tokens, info, block) do
    handle(:delete, action, tokens, block, info)
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

  defp handle(method, action, tokens, block) do
    quote do
      def unquote(action)(var!(req), var!(session_id), unquote(method), unquote(tokens)), unquote(block)
    end
  end

  defp handle(method, action, tokens, block, info) do
    quote do
      def unquote(action)(var!(req), var!(session_id), unquote(method), unquote(tokens), unquote(info)), unquote(block)
    end
  end
end
