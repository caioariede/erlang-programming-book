-module(indexing).
-export([text_to_raw/1,raw_to_document/1,line_to_words/1]).

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

%%index([Line, Document]) ->
%%    index_line(Line, Acc);
%%
%%index_line(Line, Acc) ->
%%    WordsLine = line_to_words(Line)
