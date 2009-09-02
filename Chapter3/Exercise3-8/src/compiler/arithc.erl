-module(arithc).
-export([file/1,exec/1]).

file(Filename) ->
    Output = filename:basename(Filename, ".ar"),
    {ok, Data} = file:read_file(Filename),
    compile(Output, binary_to_list(Data)).

exec(String) ->
    compile("tmp", String),
    Result = tmp:main(),
    file:delete("tmp.beam"),
    Result.

lexer(String) ->
    {_,Tokens,_} = arith_lexer:string(String),
    Tokens.

parser(String) ->
    Tokens = lexer(String),
    {ok, Exps} = arith_parser:parse(Tokens),
    Exps.

scan(String) ->
    parser(String).

form(String) ->
    Exps = scan(String),
    {_, Form} = compile_exp(Exps),
    Form.

compile(Output, String) ->
    Form = form(String),
    file:write_file(Output ++ ".beam", make_ast(Output, Form)).

compile_exp(Exps) ->
    compile_exp_acc(Exps, []).

compile_exp_acc([], Acc) ->
    {ok, lists:reverse(Acc)};

compile_exp_acc([Exp | Exps], Acc) ->
    compile_exp_acc(Exps, [transform_exp(Exp) | Acc]).

make_ast(Output, Form) ->
    Forms = [
        {attribute, 1, file, {list_to_atom(Output), 1}},
        {attribute, 1, module, list_to_atom(Output)},
        {attribute, 1, export, [{main,0}]},
        {function,  1, main, 0,
            [{clause, 1, [], [], Form}]},
        {eof, 2}],
    {ok, _, Binary} = compile:forms(Forms),
    Binary.

% transform: if
transform_exp({'if', L, X, Y}) ->
    {'if', L, [
        {clause, L, [], [[{op, L, '/=', transform_exp(X), transform_exp({num, L, 0})}]], [transform_exp(Y)]},
        {clause, L, [], [[{atom, L, true}]], [{atom, L, false}]}
    ]};

% transform: elseif
transform_exp({'ifelse', L, X, Y, Z}) ->
    {'if', L, [
        {clause, L, [], [[{op, L, '/=', transform_exp(X), transform_exp({num, L, 0})}]], [transform_exp(Y)]},
        {clause, L, [], [[{atom, L, true}]], [transform_exp(Z)]}
    ]};

% transform: var
transform_exp({identifier, L, Name}) ->
    {var, L, Name};

% transform: let
transform_exp({{identifier, L, Name}, Value}) ->
    {match, L, {var, L, Name}, transform_exp(Value)};

% transform: multi
transform_exp({{multi, L}, {num, L, 0}, {num, _, _}}) -> transform_exp({num, L, 0});
transform_exp({{multi, L}, {num, _, _}, {num, L, 0}}) -> transform_exp({num, L, 0});
transform_exp({{multi, L}, {num, _, 1}, {num, L, Y}}) -> transform_exp({num, L, Y});
transform_exp({{multi, L}, {num, L, X}, {num, _, 1}}) -> transform_exp({num, L, X});
transform_exp({{multi, L}, X, Y}) -> {op, L, '*', transform_exp(X), transform_exp(Y)};

% transform: plus
transform_exp({{plus, _}, {num, _, 0}, {num, L, Y}}) -> transform_exp({num, L, Y});
transform_exp({{plus, _}, {num, L, X}, {num, _, 0}}) -> transform_exp({num, L, X});
transform_exp({{plus, L}, X, Y}) -> {op, L, '+', transform_exp(X), transform_exp(Y)};

% transform: minus
transform_exp({{minus, _}, {num, _, 0}, {num, L, Y}}) -> transform_exp({num, L, Y});
transform_exp({{minus, _}, {num, L, X}, {num, _, 0}}) -> transform_exp({num, L, X});
transform_exp({{minus, L}, X, Y}) -> {op, L, '-', transform_exp(X), transform_exp(Y)};

% transform: minus (unary)
transform_exp({{minus, L}, Exp}) -> {op, L, '-', transform_exp(Exp)};

% transform: number
transform_exp({num, L, X}) -> {integer, L, X}.
