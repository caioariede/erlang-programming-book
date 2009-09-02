Definitions.

Digit = [0-9]
UpperCase = [A-Z]
LowerCase = [a-z]
Whitespace = [\s]
Comment = #.*

Rules.

%% Numbers
{Digit}+\.{Digit}+ : build_float(TokenChars, TokenLine).
{Digit}+ : build_integer(TokenChars, TokenLine).

%% Variables
{LowerCase}+ : build_identifier(TokenChars, TokenLine).

%% Ignored
{Comment} : skip_token.
{Whitespace}+ : skip_token.

%% Newline-spanning whitespace
({Comment}|{Whitespace})*(\n({Comment}|{Whitespace})*)+ : {token,{eol,TokenLine}}.

\(  : {token,{'(',TokenLine}}.
\)  : {token,{')',TokenLine}}.
~   : {token,{'~',TokenLine}}.
\+  : {token,{'+',TokenLine}}.
\*  : {token,{'*',TokenLine}}.
-   : {token,{'-',TokenLine}}.
=   : {token,{'=',TokenLine}}.
,   : {token,{'eol',TokenLine}}.
let : {token,{'let',TokenLine}}.

Erlang code.

build_integer(Chars, Line) ->
  {token, {integer, Line, list_to_integer(Chars)}}.
 
build_float(Chars, Line) ->
  {token, {float, Line, list_to_float(Chars)}}.

build_identifier(Chars, Line) ->
    Atom = list_to_atom(Chars),
    case reserved_word(Atom) of
        true -> {token, {Atom, Line}};
        false -> {token, {identifier, Line, Atom}}
    end.

reserved_word('if') -> true;
reserved_word('then') -> true;
reserved_word('else') -> true;
reserved_word('let') -> true;
reserved_word(_) -> false.
