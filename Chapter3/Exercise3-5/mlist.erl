-module(mlist).
-export([filter/2,reverse/1,concatenate/1,flatten/1]).

filter_acc([], _, R) ->
    R;

filter_acc([H | T], N, R) when H =< N ->
    filter_acc(T, N, R ++ [H]);

filter_acc([_ | T], N, R) ->
    filter_acc(T, N, R).

filter(List, N) ->
    filter_acc(List, N, []).

reverse_acc([], Reverse) ->
    Reverse;

reverse_acc([H | T], Reverse) ->
    reverse_acc(T, [H | Reverse]).

reverse([]) ->
    [];

reverse(List) ->
    reverse_acc(List, []).

concatenate_acc([], R) ->
    R;

concatenate_acc([H | T], R) ->
    concatenate_acc(T, R ++ H).

concatenate([]) ->
    [];

concatenate(List) ->
    concatenate_acc(List, []).

flatten_acc([], A) ->
    concatenate(A);

flatten_acc([H | T], _) ->
    flatten_acc(H, []) ++ flatten_acc(T, []);

flatten_acc(H, A) ->
    [H | A].

flatten([]) ->
    [];

flatten(List) ->
    flatten_acc(List, []).
