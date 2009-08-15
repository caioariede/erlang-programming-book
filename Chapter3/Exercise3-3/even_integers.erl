-module(even_integers).
-export([even_integers/1]).

even_integers(N) when N > 0 ->
    even_integers(1, N).

even_integers(N, M) when N == M, N rem 2 == 0 ->
    print_even(N);

even_integers(N, M) when N == M ->
    ok;

even_integers(N, M) when N rem 2 == 0 ->
    print_even(N),
    even_integers(N + 1, M);

even_integers(N, M) ->
    even_integers(N + 1, M).

print_even(N) ->
    io:format("Even: ~p~n", [N]).
