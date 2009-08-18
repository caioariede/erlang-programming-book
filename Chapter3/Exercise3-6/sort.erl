-module(sort).
-export([quicksort/1,mergesort/1]).

quicksort([]) ->
    [];

quicksort([L]) ->
    [L];

quicksort([Pivot | L]) ->
    quicksort([LT || LT <- L, LT < Pivot]) ++ [Pivot] ++ quicksort([GT || GT <- L, GT >= Pivot]).

split_acc([], Left, Right) ->
    {Left, Right};

split_acc([L], Left, Right) ->
    split_acc([], [L | Left], Right);

split_acc([L | [R | T]], Left, Right) ->
    split_acc(T, [L | Left], [R | Right]).

split(List) ->
    split_acc(List, [], []).

merge_acc([], [], Acc) ->
    lists:reverse(Acc);

merge_acc([], [R | Right], Acc) ->
    merge_acc([], Right, [R | Acc]);

merge_acc([L | Left], [], Acc) ->
    merge_acc(Left, [], [L | Acc]);

merge_acc([L | Left], [R | _] = Right, Acc) when L < R ->
    merge_acc(Left, Right, [L | Acc]);

merge_acc(Left, [R | Right], Acc) ->
    merge_acc(Left, Right, [R | Acc]).

merge(Left, Right) ->
    merge_acc(Left, Right, []).

mergesort([]) ->
    [];

mergesort([L]) ->
    [L];

mergesort(List) ->
    {Left, Right} = split(List),
    merge(mergesort(Left), mergesort(Right)).
