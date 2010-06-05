% Assertion helper functions
-module(boss_assert).
-compile(export_all).

http_ok({Status, _, _, _}) ->
  {Status =:= 200, "HTTP Status not OK"}.

http_redirect({Status, _, _, _}) ->
  {Status =:= 302, "HTTP Status not Redirect"}.

redirect_location(Url, {_, _, Headers, _}) ->
  {proplists:get_value("Location", Headers) =:= Url,
    "Did not redirect to "++Url}.

tag_with_text(Tag, Text, {_, _, _, ParseTree}) ->
    {has_tag_with_text(Tag, Text, ParseTree), "No <"++Tag++"> tag containing \""++Text++"\""}.

link_with_text(Text, {_, _, _, Response}) ->
    {boss_test:find_link_with_text(Text, Response) =/= undefined,
        "No link to \""++Text++"\""}.

has_tag_with_text(Tag, Text, ParseTree) when is_list(Tag) ->
    has_tag_with_text(list_to_binary(Tag), Text, ParseTree);
has_tag_with_text(Tag, Text, ParseTree) when is_list(Text) ->
    has_tag_with_text(Tag, list_to_binary(Text), ParseTree);
has_tag_with_text(Tag, Text, ParseTree) when is_tuple(ParseTree) ->
    has_tag_with_text(Tag, Text, [ParseTree]);
has_tag_with_text(_, _, []) ->
    false;
has_tag_with_text(Tag, Text, [SomeText|Rest]) when is_binary(SomeText) ->
    has_tag_with_text(Tag, Text, Rest);
has_tag_with_text(Tag, Text, [{Tag, _, [Text]}|_Rest]) ->
    true;
has_tag_with_text(Tag, Text, [{_OtherTag, _, Children}|Rest]) ->
    has_tag_with_text(Tag, Text, Children) orelse has_tag_with_text(Tag, Text, Rest).
