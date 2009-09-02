-module(arith_test).
-export([simple_expr/0,if_expr/0,ifelse_expr/0,let_expr/0]).
-include_lib("eunit/include/eunit.hrl").

simple_expr() ->
    ?assertEqual(arithc:exec("~((2+3)-4)"), -1).

if_expr() ->
    ?assertEqual(arithc:exec("if (1+1) then 2"), 2).

ifelse_expr() ->
    ?assertEqual(arithc:exec("if (1-1) then 2 else 0"), 0).

let_expr() ->
    ?assertEqual(arithc:exec("let c = 2, c*3"), 6).
