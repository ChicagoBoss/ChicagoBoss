Definitions.
L = [A-Za-z_\.]
D = [0-9]
F = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
HEX = 0x[0-9]+
WS  = ([\000-\s]|%.*)
S = [\(\)\]\[\{\};=]

Rules.

{L}({L}|{D})* : {token, {var, TokenLine,list_to_atom(TokenChars)}}.
'({L}|{D})+' : S = strip(TokenChars,TokenLen),
         {token,{string,TokenLine,S}}.
"({L}|{D}|/)+" : S = strip(TokenChars,TokenLen),
         {token,{string,TokenLine,S}}.
{S} : {token, {list_to_atom(TokenChars),TokenLine}}.
{WS}+  : skip_token.
//.* : skip_token.
/\*([^\*]|\*[^/])*\*/ : skip_token.
{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{F} : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{HEX} : {token, {integer, TokenLine, hex_to_int(TokenChars)}}.

Erlang code.
strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).

hex_to_int([_,_|R]) ->
    {ok,[Int],[]} = io_lib:fread("~16u", R),
    Int.
    
