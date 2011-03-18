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
    File

    Elements
    FormTokens
    Token

    Macro
    MacroName
    MacroString
    MacroArgs
    NonEmptyMacroArgs
    ApplyMacroArgs
    NonEmptyApplyMacroArgs

    Expression
    NonEmptyExpression
    ExpressionList
    NonEmptyExpressionList
    ExpressionToken.

% Terminals taken from erl_parse.yrl
Terminals
    ifdef_keyword else_keyword ifndef_keyword endif_keyword undef_keyword
    define_keyword include_keyword include_lib_keyword eof

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

Rootsymbol File.

File -> Elements eof : '$1' ++ ['$2'].
File -> Elements : '$1' ++ [{eof, 0}].

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
FormTokens -> FormTokens Macro : '$1' ++ ['$2'].
FormTokens -> FormTokens MacroString : '$1' ++ ['$2'].

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

MacroString -> '?' '?' var : {'macro_string', '$3'}.

MacroName -> atom : '$1'.
MacroName -> var : '$1'.

MacroArgs -> '$empty' : [].
MacroArgs -> NonEmptyMacroArgs : '$1'.

NonEmptyMacroArgs -> NonEmptyMacroArgs ',' var : '$1' ++ [['$3']].
NonEmptyMacroArgs -> var : [['$1']].

ApplyMacroArgs -> '$empty' : [].
ApplyMacroArgs -> NonEmptyApplyMacroArgs : '$1'.

NonEmptyApplyMacroArgs -> NonEmptyExpression : ['$1'].
NonEmptyApplyMacroArgs -> NonEmptyApplyMacroArgs ',' NonEmptyExpression : '$1' ++ ['$3'].

NonEmptyExpression -> Expression ExpressionToken : '$1' ++ ['$2'].
NonEmptyExpression -> Expression Macro : '$1' ++ ['$2'].
NonEmptyExpression -> Expression MacroString : '$1' ++ ['$2'].
NonEmptyExpression -> Expression '(' ExpressionList ')' : '$1' ++ ['$2'] ++ '$3' ++ ['$4'].
NonEmptyExpression -> Expression '{' ExpressionList '}' : '$1' ++ ['$2'] ++ '$3' ++ ['$4'].
NonEmptyExpression -> Expression '[' ExpressionList ']' : '$1' ++ ['$2'] ++ '$3' ++ ['$4'].

Expression -> '$empty' : [].
Expression -> NonEmptyExpression : '$1'.

ExpressionList -> '$empty' : [].
ExpressionList -> NonEmptyExpressionList : '$1'.

NonEmptyExpressionList -> NonEmptyExpressionList ',' NonEmptyExpression : '$1' ++ ['$2'] ++ '$3'.
NonEmptyExpressionList -> NonEmptyExpression : '$1'.

Token -> '(' : '$1'.
Token -> ')' : '$1'.
Token -> ',' : '$1'.
Token -> '{' : '$1'.
Token -> '}' : '$1'.
Token -> '[' : '$1'.
Token -> ']' : '$1'.
Token -> ExpressionToken : '$1'.

ExpressionToken -> char : '$1'.
ExpressionToken -> integer : '$1'.
ExpressionToken -> float : '$1'.
ExpressionToken -> atom : '$1'.
ExpressionToken -> string : '$1'.
ExpressionToken -> var : '$1'.
ExpressionToken -> '->' : '$1'.
ExpressionToken -> ':-' : '$1'.
ExpressionToken -> '|' : '$1'.
ExpressionToken -> '||' : '$1'.
ExpressionToken -> '<-' : '$1'.
ExpressionToken -> ';' : '$1'.
ExpressionToken -> ':' : '$1'.
ExpressionToken -> '#' : '$1'.
ExpressionToken -> '.' : '$1'.
ExpressionToken -> 'after' : '$1'.
ExpressionToken -> 'begin' : '$1'.
ExpressionToken -> 'case' : '$1'.
ExpressionToken -> 'try' : '$1'.
ExpressionToken -> 'catch' : '$1'.
ExpressionToken -> 'end' : '$1'.
ExpressionToken -> 'fun' : '$1'.
ExpressionToken -> 'if' : '$1'.
ExpressionToken -> 'of' : '$1'.
ExpressionToken -> 'receive' : '$1'.
ExpressionToken -> 'when' : '$1'.
ExpressionToken -> 'andalso' : '$1'.
ExpressionToken -> 'orelse' : '$1'.
ExpressionToken -> 'query' : '$1'.
ExpressionToken -> 'spec' : '$1'.
ExpressionToken -> 'bnot' : '$1'.
ExpressionToken -> 'not' : '$1'.
ExpressionToken -> '*' : '$1'.
ExpressionToken -> '/' : '$1'.
ExpressionToken -> 'div' : '$1'.
ExpressionToken -> 'rem' : '$1'.
ExpressionToken -> 'band' : '$1'.
ExpressionToken -> 'and' : '$1'.
ExpressionToken -> '+' : '$1'.
ExpressionToken -> '-' : '$1'.
ExpressionToken -> 'bor' : '$1'.
ExpressionToken -> 'bxor' : '$1'.
ExpressionToken -> 'bsl' : '$1'.
ExpressionToken -> 'bsr' : '$1'.
ExpressionToken -> 'or' : '$1'.
ExpressionToken -> 'xor' : '$1'.
ExpressionToken -> '++' : '$1'.
ExpressionToken -> '--' : '$1'.
ExpressionToken -> '==' : '$1'.
ExpressionToken -> '/=' : '$1'.
ExpressionToken -> '=<' : '$1'.
ExpressionToken -> '<' : '$1'.
ExpressionToken -> '>=' : '$1'.
ExpressionToken -> '>' : '$1'.
ExpressionToken -> '=:=' : '$1'.
ExpressionToken -> '=/=' : '$1'.
ExpressionToken -> '<=' : '$1'.
ExpressionToken -> '<<' : '$1'.
ExpressionToken -> '>>' : '$1'.
ExpressionToken -> '!' : '$1'.
ExpressionToken -> '=' : '$1'.
ExpressionToken -> '::' : '$1'.
