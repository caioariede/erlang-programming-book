-module(boolean).
-export([b_not/1,b_and/2,b_or/2,b_nand/2]).

b_not(false) ->
    true;

b_not(_Value) ->
    false.

b_and(true, true) ->
    true;

b_and(_Value1, _Value2) ->
    false.

b_or(true, true) ->
    true;

b_or(true, false) ->
    true;

b_or(false, true) ->
    true;

b_or(_Value1, _Value2) ->
    false.

b_nand(Value1, Value2) ->
    boolean:b_and(Value1, Value2) == false.
