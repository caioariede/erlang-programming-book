%% @Author Caio Ariede

Nonterminals
    grammar
    number
    unary
    op
    expr
    if_stmt
    if_then
    if_else
    let_stmt
    variable
    data
    .

Terminals
    '~' '+' '-' '*' '(' ')' '='
    atom
    if
    integer
    float
    let
    .

Rootsymbol
    grammar
    .

grammar -> expr     : ['$1'].
grammar -> if_stmt  : ['$1'].
grammar -> let_stmt : ['$1'].

if_stmt -> if expr if_then expr : {'if', '$2', '$4'}.
if_stmt -> if expr if_then expr if_else expr : {'ifelse', '$2', '$4', '$6'}.

let_stmt -> let variable '=' expr : {'$2', '$4'}.

number -> integer : {'num', value_of('$1')}.
number -> float : {'num', value_of('$1')}.

number -> '-' integer : {'minus', {'num', value_of('$2')}}.
number -> '-' float : {'minus', {'num', value_of('$2')}}.

expr -> data : '$1'.
expr -> data op data : {'$2', '$1', '$3'}.
expr -> expr op expr : {'$2', '$1', '$3'}.
expr -> '(' expr ')' : '$2'.
expr -> unary expr : {'$1', '$2'}.

unary -> '~' : 'minus'.

op -> '+' : 'plus'.
op -> '-' : 'minus'.
op -> '*' : 'multi'.

data -> number : '$1'.
data -> variable : '$1'.

% define some atoms as tokens
if_then -> atom : '$1'.
if_else -> atom : '$1'.
variable -> atom : {'var', value_of('$1')}.

Erlang code.

value_of(Token) ->
    element(3, Token).
