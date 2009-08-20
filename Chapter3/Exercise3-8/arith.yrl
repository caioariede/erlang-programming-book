%%
%% @Author Caio Ariede
%%

Nonterminals
    grammar
    expr
    expr_list
    number
    op
    unary
    add
    sub
    .

Terminals
    eol
    '(' ')' '+' '-' '~'
    float integer
    .

Rootsymbol
    grammar.

grammar -> expr_list : '$1'.

expr_list -> expr               : ['$1'].
expr_list -> expr eol           : ['$1'].
expr_list -> eol expr_list      : ['$2'].
expr_list -> expr eol expr_list : ['$1','$3'].

expr -> number op number  : {'$2', '$1', '$3'}.
expr -> unary expr        : {minus, '$2'}.
expr -> expr op expr      : {'$2', '$1', '$3'}.
expr -> expr op number    : {'$2', '$1', '$3'}.
expr -> '(' expr ')'      : '$2'.

number -> integer : {num, value_of('$1')}.
number -> float   : {num, value_of('$1')}.

add   -> '+' : '$1'.
sub   -> '-' : '$1'.
unary -> '~' : '$1'.

op -> '+' : 'plus'.
op -> '-' : 'minus'.

Erlang code.

value_of(Token) ->
    element(3, Token).
