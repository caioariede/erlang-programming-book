-module(indexing).
-export([
    index/2,
    index/3]).

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
        {value, {_, Index}} -> lists:reverse(Index);
        _ -> []
    end.

index(pretty_print, Filename, Word) ->
    Index = index(Filename, Word),
    index_prettier(Index, 0, [Word ++ " "]).

index_prettier([], _, []) ->
    io:format([$\n], []);

index_prettier([], _, [C|Acc]) ->
    if is_number(C) -> io:format("~B", [C]);
        true -> io:format("~s", [C])
    end,
    index_prettier([], 0, Acc);

index_prettier([I], _, Acc) ->
    index_prettier([], 0, Acc ++ [I]);

%% 1,1,2,4,5,6,6,98,100,102
index_prettier([I|T], C, Acc) ->
    % remove next equal numbers
    % 1,1,2,... will be 1,2,...
    FRest = lists:dropwhile(fun(E) -> E == I end, T),
    F = if length(FRest) > 0 ->
            [First|Rest] = FRest,
            % the first in Tail is sequence of I? (I+1)
            if First == I + 1 ->
                % remove sequential (X+1) numbers
                % [2,4] will be [2]
                % [1,2,3,5] will be [1,2,3]
                lists:foldl(fun(N, Acc2) ->
                    case lists:last(Acc2) + 1 == N of
                        true -> Acc2 ++ [N];
                        _ -> Acc2
                    end
                end, [First], Rest);
            true ->
                []
            end;
        true -> []
    end,
    NewAcc = Acc ++ if
        % there is a sequence?
        length(F) > 0 ->
            % leave only last element of sequence
            % [2,4,5,6,...] will be [4,5,6,...]
            % [1,2,3,5,...] will be [5,...]
            L = lists:last(F),
            NewT = lists:dropwhile(fun(E) -> E =< L end, T),
            % push sequence into accumulator
            % like 1-2 or 1-3
            [I, "-", L];
        true ->
            % no sequence
            NewT = T,
            [I]
    end,
    index_prettier(NewT, C + 1, NewAcc ++ if length(NewT) > 0 -> [","]; true -> [] end).

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
            [{Word, [LineCount]} | Acc]
    end,
    index_words(Words, LineCount, NewIndex).
