-module(arith_test).
-export([scan/1,parse/1,expr/1,simple_expr/0,if_expr/0,ifelse_expr/0]).
-include_lib("eunit/include/eunit.hrl").

scan(Expr) ->
    {_,Scan,_} = erl_scan:string(Expr),
    Scan.

parse(Expr) ->
    Scan = scan(Expr),
    arith_parser:parse(Scan).

expr(Expr) ->
    {_, Parse} = parse(Expr),
    Parse.

simple_expr() ->
    ?assertEqual(expr("~((2+3)-4)"),
        [{minus,{minus,{plus,{num,2},{num,3}},{num, 4}}}]).

if_expr() ->
    ?assertEqual(expr("if (1+1) then 2"),
        [{'if',{plus,{num,1},{num,1}},{num,2}}]).

ifelse_expr() ->
    ?assertEqual(expr("if (1+1) then 2 else 0"),
        [{'if',{plus,{num,1},{num,1}},{num,2},{num,0}}]).
