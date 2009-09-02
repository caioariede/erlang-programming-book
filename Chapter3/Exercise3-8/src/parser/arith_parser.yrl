Nonterminals
    grammar
    number
    op
    exprs
    expr
    expr_if
    expr_match
    expr_op
    expr_value
    .

Terminals
    '+' '-' '*' '~' '=' '(' ')'
    integer float
    if then else
    let identifier
    eol
    .

Rootsymbol
    grammar
    .

grammar -> exprs : '$1'.

exprs -> expr : ['$1']. 
exprs -> expr eol : ['$1'].
exprs -> eol exprs : ['$2'].
exprs -> expr eol exprs : ['$1'|'$3'].

expr -> expr_if : '$1'.

expr_if -> if expr_match 'then' expr_match : {'if', line_of('$1'), '$2', '$4'}.
expr_if -> if expr_match 'then' expr_match 'else' expr_match : {'ifelse', line_of('$1'), '$2', '$4', '$6'}.
expr_if -> expr_match : '$1'.

expr_match -> let identifier '=' expr_match : {'$2', '$4'}.
expr_match -> expr_op : '$1'.

expr_op -> expr_op op expr_op : {'$2', '$1', '$3'}.
expr_op -> '(' expr_op ')' : '$2'.
expr_op -> '~' expr_op : {{'minus', line_of('$1')}, '$2'}.
expr_op -> expr_value : '$1'.

expr_value -> identifier : '$1'.
expr_value -> number : '$1'.

op -> '+' : {'plus',  line_of('$1')}.
op -> '-' : {'minus', line_of('$1')}.
op -> '*' : {'multi', line_of('$1')}.

number -> integer : {'num', line_of('$1'), value_of('$1')}.
number -> float : {'num', line_of('$1'), value_of('$1')}.

number -> '-' integer : {'num', line_of('$2'), -value_of('$2')}.
number -> '-' float : {'num', line_of('$2'), -value_of('$2')}.

Erlang code.

value_of(Token) ->
    element(3, Token).

line_of(Token) ->
    element(2, Token).
