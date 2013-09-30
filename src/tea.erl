%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea).

%% tea: interpreter for the Tea language

-export([string/1, file/1]).
-export([eval/1]).
-export([i/1]).
-export([pretty_print/1]).

-type ast() :: term().

%% API

-spec string(string()) -> {ok | error, ast()}.
string(TeaCode) ->
  {ok, Tree} = tparser:string(TeaCode),
  rework_tree(Tree).

-spec file(string()) -> {ok | error, ast()}.
file(Filename) ->
  {ok, Tree} = tparser:file(Filename),
  rework_tree(Tree).

-spec eval(ast()) -> term().
eval(T) ->
  T0 = ttransform0:transform0(T),
  T1 = ttransform1:transform1(T0),
  tcore:eval(T1,[],[],[],[],{[],self()},0).

-spec i(string()) -> term().
i(String) ->
  {ok, Tree} = string(String),
  tcache:start_link(100),
  Res = eval(Tree),
  tcache:stop(),
  Res.

-spec pretty_print (AST::ast()) -> {ok, TeaCode::string()} | {error, term()}.
pretty_print (AST) ->
    pp(AST, 0).

%% Internals

s (N) -> string:chars($ , 2 * N).   %% Prepend spaces (Tab width * N).
es (T) -> [S] = io_lib:format("~p",[T]), S.   %% Ensure T is string().

pp ({wherevar, TopExpr, VarDecls}, N) ->
    s(N) ++ pp(TopExpr, N) ++ "\n" ++
    s(N) ++ "wherevar"        "\n" ++
    s(N +1) ++ pp(VarDecls, N +1)  ++
    s(N) ++ "end"             "\n";
pp ({wheredim, Vars, DimDecls}, N) ->
    s(N) ++ pp(Vars, N)    ++ "\n" ++
    s(N) ++ "wheredim"        "\n" ++
    s(N +1) ++ pp(DimDecls, N +1)  ++
    s(N) ++ "end"             "\n";
pp ({'if', Cond, Then, Else}, N) ->
    s(N) ++ "if"                 "\n" ++
    s(N +1) ++ pp(Cond, N +1) ++ "\n" ++
    s(N) ++ "then"               "\n" ++
    s(N +1) ++ pp(Then, N +1) ++ "\n" ++
    s(N) ++ "else"               "\n" ++
    s(N +1) ++ pp(Else, N +1) ++ "\n" ++
    s(N) ++ "fi"                 "\n";
pp ({'#', Dim}, N) ->
    " #."++ pp(Dim, N) ++" ";
pp ({[_],  Dim}, N) -> %% Dimensions
    es(Dim);
pp ({t, Assocs}, N) -> %% Tuples
    s(N) ++ " ["++ string:join([pp(Assoc,N) || Assoc <- Assocs], ", ") ++"] ";
pp ({primop, Fun, [Lhs,Rhs]}, N) ->
    " " ++ pp(Lhs, N) ++ " " ++
    case Fun of
        Mod when Mod == (fun mod/2) ->
            " % ";
        fun erlang:Op/2 ->
            F = case Op of
                '=<' -> '<=';
                '=/=' -> '!=';
                Op -> Op
            end,
            atom_to_list(F)
    end ++
    " " ++ pp(Rhs, N) ++ " ";
pp ({{[_],_}=Dim, Init}, N) -> %% DimDecls
    s(N) ++ "dim "++ pp(Dim) ++" <- "++ pp(Init, N) ++ "\n";
pp ({Var, Expr}, N) -> %% VarDecls
    s(N) ++ "var "++ es(Var)  ++ " = " "\n" ++
    s(N +1) ++ pp(Expr, N +1) ++       "\n";
pp (Rest, _) ->
    es(Rest).

rework_tree (Tree) ->
    V = fun
        ({where, _, Exp, DimDecls, VarDecls}) ->
            TopExpr = Exp,
            Vars = [{var,Var, E}|| {var_decl,_,Var,E} <- VarDecls],
            Dims = [{dim,Dim,N} || {dim_decl,_,Dim,N} <- DimDecls],
            {where, TopExpr, Vars ++ Dims};

        ({'if', _, Ifs, Else}) -> unwrap_elsifs(Ifs, Else);

        ({'#.', _, Val}) ->
            %% On Section 6.4.4 “Querying the context” of the TL-doc-0.3.0
            %%   it explicitly states that ‘#.’ takes a dimension as input.
            {'#', {dim,Val}};

        ({tuple, _, Assocs}) -> {t, Assocs};
        ({tuple_element, _, Lhs, Rhs}) ->
            %% On Section 6.4.5 “Tuples” of the TL-doc-0.3.0, tuples are
            %%   defined as a ‘set of (dimension, value) pairs’.
            {{dim,Lhs}, Rhs};

        ({'or'=Op, _, A, B})  -> {primop, fun erlang:Op/2, [A,B]};
        ({'and'=Op, _, A, B}) -> {primop, fun erlang:Op/2, [A,B]};
        ({'<'=Op, _, A, B})   -> {primop, fun erlang:Op/2, [A,B]};
        ({'<=', _, A, B})     -> {primop, fun erlang:'=<'/2, [A,B]};
        ({'=='=Op, _, A, B})  -> {primop, fun erlang:Op/2, [A,B]};
        ({'>='=Op, _, A, B})  -> {primop, fun erlang:Op/2, [A,B]};
        ({'>'=Op, _, A, B})   -> {primop, fun erlang:Op/2, [A,B]};
        ({'!=', _, A, B})     -> {primop, fun erlang:'=/='/2, [A,B]};
        ({'+'=Op, _, A, B})   -> {primop, fun erlang:Op/2, [A,B]};
        ({'-'=Op, _, A, B})   -> {primop, fun erlang:Op/2, [A,B]};
        ({'*'=Op, _, A, B})   -> {primop, fun erlang:Op/2, [A,B]};
        % No '/' yet because lack of floats.
        ({'%', _, A, B})      -> {primop, fun mod/2, [A,B]};

        ({bool, _, Boolean}) -> Boolean;
        ({raw_string, _, S})    -> {string, S};
        ({cooked_string, _, S}) -> {string, S};
        ({char, _, Char}) -> {char, Char};

        ({'@', _, A, B}) -> {'@', A, B};

        ({int,_,N}) -> N;
        ({float,_,N}) -> N;
        ({id,_,Name}) -> Name
    end,
    case tvisitor:visit(V, Tree, bottom_up) of
        [{expr,_,TheWhereDim}] -> {ok, TheWhereDim}
        ; Else -> {ok, Else}
    end.


unwrap_elsifs ([{if_expr,_,Cond,Then}|Rest], Else) ->
    {'if', Cond, Then, unwrap_elsifs(Rest,Else)};
unwrap_elsifs ([], Else) -> Else.

mod (X, Y) ->
%% http://stackoverflow.com/a/858649/1418165
    (X rem Y + Y) rem Y.

%% End of Module.
