-module(indexing).
-export([
    text_to_raw/1,
    raw_to_document/1,
    line_to_words/1,
    index/2,
    index/3,
    index_prettier/3]).

-define(MAX_LINE_LEN, 100).

text_to_raw([], _, _, AccLine) ->
    AccLine;

text_to_raw([$\n|Source], CharCount, AccChar, AccLine) ->
    text_to_raw(Source, CharCount, AccChar, AccLine);

text_to_raw([$ |Source], CharCount, AccChar, AccLine) when CharCount > ?MAX_LINE_LEN ->
    text_to_raw(Source, 1, [], AccLine ++ [AccChar]);

text_to_raw([Char|Source], CharCount, AccChar, AccLine) ->
    text_to_raw(Source, CharCount + 1, AccChar ++ [Char], AccLine).

text_to_raw(Filename) ->
    {ok, Source} = file:read_file(Filename),
    text_to_raw(binary_to_list(Source), 1, [], []).

raw_to_document([]) ->
    [];

raw_to_document([L|Lines]) ->
    raw_to_document(Lines, [line_to_words(L)]).

raw_to_document([], Acc) ->
    lists:reverse(Acc);

raw_to_document([L|Lines], Acc) ->
    raw_to_document(Lines, [line_to_words(L) | Acc]).

line_to_words(Line) ->
    line_to_words(Line, []).

line_to_words([], Acc) ->
    string:tokens(Acc, [$ ]);

line_to_words([C|Line], Acc) when (C == $.) or (C == $,) ->
    line_to_words(Line, Acc);

line_to_words([C|Line], Acc) ->
    line_to_words(Line, Acc ++ [C]).

index(Filename, Word) ->
    Raw = text_to_raw(Filename),
    [Current | Lines] = raw_to_document(Raw),
    case lists:keysearch(Word, 1, index(Lines, Current, 1, [])) of
        {value, Index} -> Index;
        _ -> {Word, []}
    end.

index(pretty_print, Filename, Word) ->
    {_, Index} = index(Filename, Word),
    index_prettier(Index, 0, Word ++ " ").

index_prettier([], _, Acc) ->
    Acc;

%index_prettier([I|T], Previous, Acc) ->
%; // TODO

index([], Current, LineCount, Acc) ->
    index_words(Current, LineCount, Acc);

index([Next | Lines], Current, LineCount, Acc) ->
    index(Lines, Next, LineCount + 1, index_words(Current, LineCount, Acc)).

index_words([], _, Index) ->
    Index;

index_words([Word | Words], LineCount, Acc) ->
    NewIndex = case lists:keysearch(Word, 1, Acc) of
        {value, {_, Value}} ->
            lists:keyreplace(Word, 1, Acc, {Word, [LineCount | Value]});
        _ ->
            [{Word, [LineCount, -1]} | Acc]
    end,
    index_words(Words, LineCount, NewIndex).
