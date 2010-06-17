% Alternative Erlang preprocessor

Nonterminals
    IfBlock
    IfNBlock
    IfDef
    IfNDef
    Else
    EndIf
    Undef
    Define
    Include
    IncludeLib
    Elements
    FormTokens
    Token
    Macro
    MacroName
    MacroArgs
    ApplyMacroArgs
    ApplyMacroArg.

% Terminals taken from erl_parse.yrl
Terminals
    ifdef_keyword else_keyword ifndef_keyword endif_keyword undef_keyword
    define_keyword include_keyword include_lib_keyword

    '?'

    char integer float atom string var
    '(' ')' ',' '-'
    '->' ':-' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
    'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
    'andalso' 'orelse' 'query' 'spec'
    'bnot' 'not'
    '*' '/' 'div' 'rem' 'band' 'and'
    '+' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
    '++' '--'
    '==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
    '<<' '>>'
    '!' '=' '::'
    dot.

Rootsymbol Elements.

Elements -> '$empty' : [].
Elements -> Elements IfBlock : '$1' ++ ['$2'].
Elements -> Elements IfNBlock : '$1' ++ ['$2'].
Elements -> Elements Undef : '$1' ++ ['$2'].
Elements -> Elements Macro : '$1' ++ ['$2'].
Elements -> Elements Define : '$1' ++ ['$2'].
Elements -> Elements Include : '$1' ++ ['$2'].
Elements -> Elements IncludeLib : '$1' ++ ['$2'].
Elements -> Elements Token : '$1' ++ ['$2'].
Elements -> Elements dot : '$1' ++ ['$2'].

FormTokens -> '$empty' : [].
FormTokens -> FormTokens Token : '$1' ++ ['$2'].

Define -> '-' define_keyword '(' MacroName ')' dot : {'macro_define', '$4'}.
Define -> '-' define_keyword '(' MacroName ',' FormTokens ')' dot : {'macro_define', '$4', '$6'}.
Define -> '-' define_keyword '(' MacroName '(' MacroArgs ')' ',' FormTokens ')' dot : {'macro_define', '$4', '$6', '$9'}.

Include -> '-' include_keyword '(' string ')' dot : {'macro_include', '$4'}.
IncludeLib -> '-' include_lib_keyword '(' string ')' dot : {'macro_include_lib', '$4'}.

IfBlock -> IfDef Elements Else Elements EndIf : {'macro_ifdef', '$1', '$2', '$4'}.
IfBlock -> IfDef Elements EndIf : {'macro_ifdef', '$1', '$2'}.

IfNBlock -> IfNDef Elements Else Elements EndIf : {'macro_ifndef', '$1', '$2', '$4'}.
IfNBlock -> IfNDef Elements EndIf : {'macro_ifndef', '$1', '$2'}.

IfDef -> '-' ifdef_keyword '(' MacroName ')' dot : '$4'.
IfNDef -> '-' ifndef_keyword '(' MacroName ')' dot : '$4'.
Else -> '-' else_keyword dot.
EndIf -> '-' endif_keyword dot.

Undef -> '-' undef_keyword '(' MacroName ')' dot : {'macro_undef', '$4'}.

Macro -> '?' MacroName : {'macro', '$2'}.
Macro -> '?' MacroName '(' ApplyMacroArgs ')' : {'macro', '$2', '$4'}.

MacroArgs -> '$empty' : [].
MacroArgs -> MacroArgs ',' var : '$1' ++ ['$2', '$3'].
MacroArgs -> var : ['$1'].

ApplyMacroArgs -> '$empty' : [].
ApplyMacroArgs -> ApplyMacroArgs ',' ApplyMacroArg : '$1' ++ ['$2', '$3'].
ApplyMacroArgs -> ApplyMacroArg : ['$1'].

% This is weak -- we really want to allow any valid Erlang term.
% For now, just variables, macros, and literals
ApplyMacroArg -> var : '$1'.
ApplyMacroArg -> integer : '$1'.
ApplyMacroArg -> float : '$1'.
ApplyMacroArg -> string : '$1'.
ApplyMacroArg -> atom : '$1'.
ApplyMacroArg -> Macro : '$1'.

MacroName -> atom : '$1'.
MacroName -> var : '$1'.


Token -> char : '$1'.
Token -> integer : '$1'.
Token -> float : '$1'.
Token -> atom : '$1'.
Token -> string : '$1'.
Token -> var : '$1'.
Token -> '(' : '$1'.
Token -> ')' : '$1'.
Token -> ',' : '$1'.
Token -> '->' : '$1'.
Token -> ':-' : '$1'.
Token -> '{' : '$1'.
Token -> '}' : '$1'.
Token -> '[' : '$1'.
Token -> ']' : '$1'.
Token -> '|' : '$1'.
Token -> '||' : '$1'.
Token -> '<-' : '$1'.
Token -> ';' : '$1'.
Token -> ':' : '$1'.
Token -> '#' : '$1'.
Token -> '.' : '$1'.
Token -> 'after' : '$1'.
Token -> 'begin' : '$1'.
Token -> 'case' : '$1'.
Token -> 'try' : '$1'.
Token -> 'catch' : '$1'.
Token -> 'end' : '$1'.
Token -> 'fun' : '$1'.
Token -> 'if' : '$1'.
Token -> 'of' : '$1'.
Token -> 'receive' : '$1'.
Token -> 'when' : '$1'.
Token -> 'andalso' : '$1'.
Token -> 'orelse' : '$1'.
Token -> 'query' : '$1'.
Token -> 'spec' : '$1'.
Token -> 'bnot' : '$1'.
Token -> 'not' : '$1'.
Token -> '*' : '$1'.
Token -> '/' : '$1'.
Token -> 'div' : '$1'.
Token -> 'rem' : '$1'.
Token -> 'band' : '$1'.
Token -> 'and' : '$1'.
Token -> '+' : '$1'.
Token -> '-' : '$1'.
Token -> 'bor' : '$1'.
Token -> 'bxor' : '$1'.
Token -> 'bsl' : '$1'.
Token -> 'bsr' : '$1'.
Token -> 'or' : '$1'.
Token -> 'xor' : '$1'.
Token -> '++' : '$1'.
Token -> '--' : '$1'.
Token -> '==' : '$1'.
Token -> '/=' : '$1'.
Token -> '=<' : '$1'.
Token -> '<' : '$1'.
Token -> '>=' : '$1'.
Token -> '>' : '$1'.
Token -> '=:=' : '$1'.
Token -> '=/=' : '$1'.
Token -> '<=' : '$1'.
Token -> '<<' : '$1'.
Token -> '>>' : '$1'.
Token -> '!' : '$1'.
Token -> '=' : '$1'.
Token -> '::' : '$1'.
