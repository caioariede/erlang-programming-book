-module(arithc).
-export([file/1,exec/1,scan/1,form/1,compile/1]).

file(Filename) ->
    Output = filename:basename(Filename, ".ar") ++ ".beam",
    {ok, Data} = file:read_file(Filename),
    exec(binary_to_list(Data)).

exec(String) ->
    compile(String),
    code:purge(output),
    code:load_file(output),
    output:output().

scan(String) ->
    code:purge(arith_parser),
    code:load_file(arith_parser),
    {_,Tokens,_} = arith_lexer:string(String),
    {ok, Exps} = arith_parser:parse(Tokens),
    Exps.

form(String) ->
    Exps = scan(String),
    {_, Form} = compile_exp(Exps, 1),
    Form.

compile(String) ->
    Form = form(String),
    file:write_file("output.beam", make_ast(Form)).

compile_exp(Exps, Line) ->
    compile_exp_acc(Exps, Line, []).

compile_exp_acc([], _, Acc) ->
    {ok, lists:reverse(Acc)};

compile_exp_acc([Exp | Exps], Line, Acc) ->
    compile_exp_acc(Exps, Line, [transform_exp(Line, Exp) | Acc]).

% transform: if
transform_exp(L, {'if', X, Y}) ->
    {'if', L, [
        {clause, L, [], [[{op, L, '/=', transform_exp(L, X), transform_exp(L, {num, 0})}]], [transform_exp(L, Y)]},
        {clause, L, [], [[{atom, L, true}]], [{atom, L, false}]}
    ]};

% transform: elseif
transform_exp(L, {'ifelse', X, Y, Z}) ->
    {'if', L, [
        {clause, L, [], [[{op, L, '/=', transform_exp(L, X), transform_exp(L, {num, 0})}]], [transform_exp(L, Y)]},
        {clause, L, [], [[{atom, L, true}]], [transform_exp(L, Z)]}
    ]};

% transform: let
transform_exp(L, {{var, Variable}, Value}) ->
    {match, L, {var, L, Variable}, transform_exp(L, Value)};

% transform: multi
transform_exp(L, {multi, {num, 0}, {num, _}}) -> transform_exp(L, {num, 0});
transform_exp(L, {multi, {num, _}, {num, 0}}) -> transform_exp(L, {num, 0});
transform_exp(L, {multi, {num, 1}, {num, Y}}) -> transform_exp(L, {num, Y});
transform_exp(L, {multi, {num, X}, {num, 1}}) -> transform_exp(L, {num, X});
transform_exp(L, {multi, X, Y}) -> {op, L, '*', transform_exp(L, X), transform_exp(L, Y)};

% transform: plus
transform_exp(L, {plus, {num, 0}, {num, Y}}) -> transform_exp(L, {num, Y});
transform_exp(L, {plus, {num, X}, {num, 0}}) -> transform_exp(L, {num, X});
transform_exp(L, {plus, X, Y}) -> {op, L, '+', transform_exp(L, X), transform_exp(L, Y)};

% transform: minus
transform_exp(L, {minus, {num, 0}, {num, Y}}) -> transform_exp(L, {num, Y});
transform_exp(L, {minus, {num, X}, {num, 0}}) -> transform_exp(L, {num, X});
transform_exp(L, {minus, X, Y}) -> {op, L, '-', transform_exp(L, X), transform_exp(L, Y)};

% transform: minus (unary)
transform_exp(L, {minus, Exp}) -> {op, L, '-', transform_exp(L, Exp)};

% transform: number
transform_exp(L, {num, X}) -> {integer, L, X}.

make_ast(Form) ->
    Forms = [
        {attribute, 1, file, {"output.erl", 1}},
        {attribute, 1, module, output},
        {attribute, 2, export, [{output,0}]},
        {function,  3, output, 0,
            [{clause, 3, [], [], Form}]},
        {eof, 4}],
    {ok, _, Binary} = compile:forms(Forms),
    Binary.
