-module(db).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

new() ->
    [].

destroy(_) ->
    [].

write(Key, Element, Db) ->
    lists:append(Db, [{Key, Element}]).

delete(_, []) ->
    [];

delete(Key, Db) ->
    lists:keydelete(Key, 1, Db).

read(_, []) ->
    [];

read(Key, Db) ->
    lists:keysearch(Key, 1, Db).

match(_, []) ->
    [];

match(Element, Db) ->
    lists:keysearch(Element, 2, Db).
