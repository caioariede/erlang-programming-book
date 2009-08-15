-module(create).
-export([create/1,reverse_create/1]).

create(0) ->
    [];

create(N) ->
    create(N, [N]).

create(1, M) ->
    M;

create(N, M) ->
    create(N - 1, [N - 1 | M]).

reverse_create(N) ->
    reverse_create(create(N), []).

reverse_create([], List) ->
    List;

reverse_create([H | Tail], List) ->
    reverse_create(Tail, [H | List]).
