-module(print_times).
-export([print_times/1]).

print_times(N) when N > 0 ->
    print_times(1, N).

print_times(N, M) ->
    io:format("Number: ~p~n", [N]),
    if
        N < M -> print_times(N + 1, M);
        true  -> ok
    end.
