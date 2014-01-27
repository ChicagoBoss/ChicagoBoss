-module(boss_flash).
-export([get_and_clear/1, add/5, add/4, add/3]).

%% @spec get_and_clear(SessionID) -> [Message]
%% @doc Retrieve the current flash messages for `SessionID' and flush the message stack.
get_and_clear(SessionID) ->
    case boss_session:get_session_data(SessionID, boss_flash) of
        undefined -> [];
        BossFlash ->
            boss_session:remove_session_data(SessionID, boss_flash),
            [{boss_flash, lists:reverse(BossFlash)}]
    end.

%% @spec add(SessionID, Type, Title) -> ok | {error, Reason}
%% @doc Add a message to the flash message stack for `SessionID'.
add(SessionID, Type, Title) ->
    add(SessionID, Type, Title, undefined, undefined).
	
%% @spec add(SessionID, Type, Title, Message) -> ok | {error, Reason}
%% @doc Add a message to the flash message stack for `SessionID'.
add(SessionID, Type, Title, Message) ->
    add(SessionID, Type, Title, Message, undefined).

%% @spec add(SessionID, Type, Title, Message, Data) -> ok | {error, Reason}
%% @doc Add a message to the flash message stack for `SessionID'.
add(SessionID, Type, Title, Message, Data) ->
  Msg = [{method, atom_to_list(Type)}, {title, Title}, {message, Message}, {data, Data}],
  Flash = case boss_session:get_session_data(SessionID, boss_flash) of
            undefined ->
              [Msg];
            ExistingFlash ->
              [Msg|ExistingFlash]
          end,
  boss_session:set_session_data(SessionID, boss_flash, Flash).
