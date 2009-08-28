-module(arithc).
-export([exec/1,scan/1,form/1,compile/1]).

exec(String) ->
    compile(String),
    code:purge(output),
    code:load_file(output),
    output:output().

scan(String) ->
    yecc:yecc("arith.yrl", "arith.erl"),
    code:purge(arith),
    code:load_file(arith),
    compile:file(arith),
    {_,Tokens,_} = erl_scan:string(String),
    {ok, Exps} = arith:parse(Tokens),
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
