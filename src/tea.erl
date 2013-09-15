%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tea).

%% tea: interpreter for the Tea language

-export([string/1, file/1]).

%% API

-export([i/1]).%
i (String) ->
    {ok, Tree} = string(String),
    tcache:start_link(100),
    tcore:eval(Tree, [],[],[],[], [0], 0).

-spec string (string()) -> {ok | error, term()}.
string (TeaCode) ->
    {ok, Tree} = tparser:string(TeaCode),
    rework_tree(Tree).

-spec file (string()) -> {ok | error, term()}.
file (Filename) ->
    {ok, Tree} = tparser:file(Filename),
    rework_tree(Tree).

%% Internals

rework_tree (Tree) ->
    V = fun
        ({where, _, Exp, DimDecls, VarDecls}) ->
            TopExpr = Exp,
            Vars = case VarDecls of
                [] ->
                    TopExpr;
                VarDecls ->
                    {wherevar, TopExpr,
                        [{Var, E} || {var_decl,_,Var,E} <- VarDecls]}
            end,
            {wheredim, Vars,
                [{{[0],Dim},N} || {dim_decl,_,Dim,N} <- DimDecls]};

        ({'if', _, [If|_], Else}) ->
            % What about elsifs* ?
            {if_expr,_,Cond,Then} = If,
            {'if', Cond, Then, Else};

        ({'#.', _, Val}) ->
            % Assumption: Val is always a dimention (Grammar allows expr).
            {'#', {[0],Val}};

        ({tuple, _, Assocs}) -> {t, Assocs};
        ({tuple_element, _, Lhs, Rhs}) -> {{[0],Lhs}, Rhs};

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
        % No '/' yet because floats.
            %% http://stackoverflow.com/a/858649/1418165
        ({'%', _, A, B})      -> {primop, fun (X,Y) -> (X rem Y + Y) rem Y end, [A,B]};

        ({raw_string, _, S})    -> {string, S};
        ({cooked_string, _, S}) -> {string, S};

        ({'@', _, A, B}) -> {'@', A, B};

        ({int,_,N}) -> N;
        ({id,_,Name}) -> Name
    end,
    case tvisitor:visit(V, Tree, bottom_up) of
        [{expr,_,TheWhereDim}] -> {ok, TheWhereDim}
        ; Else -> {ok, Else}
    end.

%% End of Module.
