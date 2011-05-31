Nonterminals
g_protobuffs g_header g_message g_rpcs g_rpc g_element g_elements g_var g_value g_default.

Terminals ';' '=' '{' '}' '[' ']' '(' ')' string integer float var.

Rootsymbol g_protobuffs.
Endsymbol '$end'.

g_protobuffs -> '$empty'					: [].
g_protobuffs -> g_header g_protobuffs				: ['$1'|'$2'].
g_protobuffs -> g_message g_protobuffs 				: ['$1'|'$2'].

g_header -> g_var string ';'					: {'$1', unwrap('$2')}.
g_header -> g_var g_var ';'					: {'$1', safe_string('$2')}.
g_header -> g_var g_var '=' g_value ';'				: {'$1', '$2', '$4'}.

g_message -> g_var g_var '{' g_elements '}'			: {'$1', safe_string('$2'), '$4'}.
g_message -> g_var g_var '{' g_rpcs '}'				: {'$1', safe_string('$2'), '$4'}.

g_rpcs -> g_rpc							: ['$1'].
g_rpcs -> g_rpc g_rpcs						: ['$1' | '$2'].

g_rpc -> g_var g_var '(' g_var ')' g_var '(' g_var ')' ';'	: {'$1', safe_string('$2'), safe_string('$4'), safe_string('$8')}.

g_elements -> g_element						: ['$1'].
g_elements -> g_element g_elements				: ['$1' | '$2'].

g_element -> g_var g_var g_var '=' integer g_default ';'	: {unwrap('$5'), pack_repeated('$1','$6'), safe_string('$2'), safe_string('$3'), default('$6')}.
g_element -> g_var '=' integer ';'				: {'$1', unwrap('$3')}.
g_element -> g_var integer g_var integer ';' 			: {'$1', unwrap('$2'), unwrap('$4')}.
g_element -> g_var integer g_var g_var ';' 			: {'$1', unwrap('$2'), '$4'}.
g_element -> g_var g_var '=' g_value ';'			: {'$1', '$2', '$4'}.
g_element -> g_message						: '$1'.

g_var -> var 							: unwrap('$1').

g_value -> g_var						: '$1'.
g_value -> integer						: unwrap('$1').
g_value -> string						: unwrap('$1').
g_value -> float						: unwrap('$1').

g_default -> '$empty' : none.
g_default -> '[' g_var '=' g_value ']' 				: {'$2', '$4'}.

Erlang code.
safe_string(A) -> make_safe(atom_to_list(A)).

reserved_words() ->
  ["after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond", "div", "end", "fun",
   "if", "let", "not", "of", "or", "orelse", "query", "receive", "rem", "try", "when", "xor"].

make_safe(String) ->
  case lists:any(fun(Elem) -> string:equal(String,Elem) end, reserved_words()) of 
    true -> "pb_"++String;
    false -> String
  end.

unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

default({default,D}) ->
  D;
default(_) ->
  none.

pack_repeated(repeated,{packed,true}) ->
  repeated_packed;
pack_repeated(Type,_) ->
  Type.
