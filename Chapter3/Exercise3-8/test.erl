-module(test).
-export([scan/1,parse/1,expr/1,simple_expr/0,if_expr/0,ifelse_expr/0]).
-include_lib("eunit/include/eunit.hrl").

scan(Expr) ->
    {_,Scan,_} = erl_scan:string(Expr),
    Scan.

parse(Expr) ->
    yecc:yecc("arith.yrl", "arith.erl"),
    code:purge(arith),
    code:load_file(arith),
    compile:file(arith),
    Scan = scan(Expr),
    arith:parse(Scan).

expr(Expr) ->
    {_, Parse} = parse(Expr),
    Parse.

simple_expr() ->
    ?assertEqual(expr("((2+3)-4)"),
        {minus,{plus,{num,2},{num,3}},{num, 4}}).

if_expr() ->
    ?assertEqual(expr("if (1+1) then 2"),
        {'if',{plus,{num,1},{num,1}},{num,2}}).

ifelse_expr() ->
    ?assertEqual(expr("if (1+1) then 2 else 0"),
        {'if',{plus,{num,1},{num,1}},{num,2},{num,0}}).
