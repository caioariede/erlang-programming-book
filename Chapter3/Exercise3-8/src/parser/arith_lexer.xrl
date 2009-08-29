Definitions.

Digit = [0-9]

Rules.

%% Numbers
-?{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
-?{Digit}+ : build_integer(TokenChars, TokenLine).

\( : {token,{'(',TokenLine}}.
\) : {token,{')',TokenLine}}.
\+ : {token,{'+',TokenLine}}.
;  : {token,{'eol',TokenLine}}.

Erlang code.

build_integer([$-|Chars], Line) ->
  {token, {integer, Line, -list_to_integer(Chars)}};
build_integer(Chars, Line) ->
  {token, {integer, Line, list_to_integer(Chars)}}.
 
build_float([$-|Chars], Line) ->
  {token, {float, Line, -list_to_float(Chars)}};
build_float(Chars, Line) ->
  {token, {float, Line, list_to_float(Chars)}}.
