-module(ice_runtime).

-export([eval/6]).

%%------------------------------------------------------------------------------
%% @doc Evaluates a sequence of expressions sequentially
%%------------------------------------------------------------------------------
eval_seq(Exps, I, E, K, D, W) ->
  eval_seq(Exps, I, E, K, D, W, []).

eval_seq([], I, E, K, D, W, Acc) ->
  hd(Acc);
eval_seq([Exp|Exps], I, E, K, D, W, Acc) ->
  Di = eval(Exp, I, E, K, D, W),
  %% Fixme: Check missing dimensions before progressing?
  eval_seq(Exps, I, E, K, D, W, [Di|Acc]).

%%------------------------------------------------------------------------------
%% @doc Evaluates a sequence of expressions in parallel
%%------------------------------------------------------------------------------
eval_par(Xs, I, E, K, D, W, T) ->
  Lim = length(Xs),
  %%------------------------------------------------------------------------------
  %% Note: Passing self() here means W is self()
  %%------------------------------------------------------------------------------
  Pids = ice_thread:spawn_n(W, Lim),
  ice_thread:join(Pids, Xs, I, E, K, D, W, T).

%%------------------------------------------------------------------------------
%% @doc Evaluates an expression
%%------------------------------------------------------------------------------

%% Evaluates a primitive operation
eval({primop, F, Eis}, I, E, K, D, W) ->
  Dis0 = eval_par(Eis, I, E, K, D, W),
  case ice_sets:dim_union(Dis0) of
    {true, Dims} ->
      Dims;
    {false, Dis1} ->
      apply(ice_primop_eval, F, Dis1)
  end;

%% Evaluates a tuple
eval({tuple, Eis}, I, E, K, D, W) ->
  XiEis = lists:flatmap(fun ({Xi, Ei}) -> [Xi, Ei] end, Eis),
  Dis0 = eval_par(XiEis, I, E, K, D, W),
  case ice_sets:dim_union(Dis0) of
    {true, Dims} ->
      Dims;
    {false, Dis1} ->
      Tuple = lists:zip(odd_elems(Dis1), even_elems(Dis1)),
      {tuple, Tuple}
  end;

%% Evaluates a context perturbation
eval({'@', E0, E1}, I, E, K, D, W) ->
  Di0 = eval(E1, I, E, K, D, W),
  case ice_sets:is_context(Di0) of
    true ->
      lists:filter(fun ice_sets:is_domain/1, Di0);
    false ->
      {tuple, Di1} = Di0,
      Ki  = ice_sets:perturb(K, Di1),
      Di2 = ice_sets:union(D, ice_sets:domain(Di1)),
      eval(E0, I, E, Ki, Di2, W)
  end;

%% Evaluates a conditional
eval({'if', E0, E1, E2}, I, E, K, D, W) ->
  Di = eval(E0, I, E, K, D, W),
  case ice_sets:is_context(Di) of
    true ->
      Di;
    false ->
      case Di of
	true ->
	  eval(E1, I, E, K, D, W);
	false ->
	  eval(E2, I, E, K, D, W)
      end
  end;

%% Evaluates a dimensional query or literal dimension
eval({Q, E0}, I, E, K, D, W) when Q == '#' orelse Q == '?' ->
  Di = eval(E0, I, E, K, D, W),
  case ice_sets:is_context(Di) of
    true ->
      Di;
    false ->
      case lists:member(Di, D) of
	true ->
	  case {Q, Di} of
	    {'?', {phi, _}} ->
	      lookup_ordinate(Di, K);
	    {'#', {dim, _, _}} ->
	      lookup_ordinate(Di, K);
	    {'#', _} ->
	      io:format("Querying context ~p for dimension ~p~n", [Q, Di]),
	      lookup_ordinate(Di, K)
	  end;
	false ->
	  [Di]
      end
  end;

%% Evaluates a dimensional identifier 'dim'
eval({dim, {_Pos, _Index}, Xi} = Di, _I, _E, _K, _D, _W) 
  when is_list(Xi) orelse is_atom(Xi) ->
  Di;

%% Evaluates a unique dimensional identifier 'phi'
eval({phi, Xi} = Di, _I, _E, _K, _D, _W)
  when is_list(Xi) orelse is_atom(Xi) ->
  Di;

%% Evaluates a wherevar clause
eval({wherevar, E0, XiEis}, I, E, K, D, W, T) ->
  %% Close shallowest abstractions in new expressions in environment
  %% if needed. Wherevar is the only expression changing the
  %% environment, therefore the only one needing to do this.
  XiClEis = ice_closure:close_shallowest_abs_in_wherevar_expressions(XiEis, I, E),
  eval(E0, I, ice_sets:perturb(E, XiClEis), K, D, W, T);

%% Evaluates a wheredim clause
eval({wheredim, E0, XiEis}, I, E, K, D, W, T) ->
  {Xis, Eis} = lists:unzip(XiEis),
  Dis = eval_par(Eis, I, E, K, D, W, T),
  case ice_sets:dim_union(Dis) of
    {true, Dims} ->
      Dims;
    {false, Dis} ->
      Ki1 = ice_sets:perturb(K, lists:zip(Xis, Dis)),
      %% XXX It is unclear if legal or illegal programs violating the
      %% following hardcoded expectation exist.
      [] = ice_sets:intersection(D, Xis),
      %% The hidden dimensions shall be added by the wheredim rule to
      %% the set of known dimensions (the rule in the paper
      %% "Multidimensional Infinite Data in the Language Lucid", Feb
      %% 2013, needs this correction re Delta) otherwise the body
      %% cannot use them.
      Di1 = ice_sets:union(D, Xis),
      eval(E0, I, E, Ki1, Di1, W, MaxT)
  end;

%% Evaluates a base abstraction
eval({b_abs, _Is, _Params, _E0} = Abs, I, E, K, D, W) ->
  freeze(Abs, I, E, K, D, W);

%% Evaluates a base application
eval({b_apply, E0, Eis}, I, E, K0, D, W) ->
  Di0_Dis = eval_par([E0|Eis], I, E, K0, D, W),
  case ice_sets:dim_union(Dis) of
    {true, Dims} ->
      Dims;
    {false, Di0_Dis} ->
      [Di0|Dis] = Di0_Dis,
      {b_abs, ClI, ClE, FrozenK, AbsParams, AbsBody} = Di0,
      AbsParamsK = lists:zip(AbsParams, Dis),
      K1 = ice_sets:perturb(FrozenK, AbsParamsK),
      eval(AbsBody, ClI, ClE, K1, ice_sets:domain(K1), W)
  end;

%% Evaluates a value abstraction
eval({v_abs, _Is, _Params, _E0} = Abs, I, E, K, D, W) ->
  freeze(Abs, I, E, K, D, W);

%% Evaluates a value application
eval({v_apply, E0, Eis}, I, E, K0, D0, W) ->
  Di0_Dis = par_eval([E0|Eis], I, E, K0, D0, W),
  case ice_sets:dim_union(Dis) of
    {true, Dims} ->
      Dims;
    {false, Di0_Dis} ->
      [Di0|Dis] = Di0_Dis,
      {v_abs, ClI, ClE, FrozenK, AbsParams, AbsBody} = Di0,
      AbsParamsK = lists:zip(AbsParams, Dis),
      FrozenParamsK = ice_sets:perturb(FrozenK, AbsParamsK),
      K1 = ice_sets:perturb(FrozenParamsK, AbsParamsK),
      D1 = ice_sets:union(D0, ice_sets:domain(FrozenParamsK)),
      eval(AbsBody, ClI, ClE, K1, D1, W)
  end;

%% Evaluates an intension abstraction
eval({i_abs, _Is, _E0} = Abs, I, E, K, D, W) ->
  freeze(Abs, I, E, K, D, W);

%% Evaluates an intension application
eval({i_apply, E0}, I, E, K0, D0, W) ->
  Di = eval(E0, I, E, K0, D0, W),
  case ice_sets:is_context(Di) of
    true ->
      Di;
    false ->
      {i_abs, ClI, ClE, FrozenK, AbsBody} = Di,
      K1 = ice_sets:perturb(K0, FrozenK),
      D1 = ice_sets:union(D0, ice_sets:domain(FrozenK)),
      eval(AbsBody, ClI, ClE, K1, D1, W)
  end;

%% Evaluates a lambda expression

%% Evaluates a variable identifier
eval(Xi, I, E, K, D, W, T) when is_list(Xi) orelse is_atom(Xi) ->
  %% This rule differs from the one described in the Feb 2013 cache
  %% semantics paper in order to avooid to return a calc value in case
  %% of GC concurrent with the invocation of beta.find().
  %%
  %% XXX Can GC be concurrent in the first place? Answer via email by
  %% John Plaice indicates that "the collect cannot be run
  %% simultaneously".
  %%
  %% The rule as per Feb 2013 cache semantics paper has (or at least
  %% Luca thinks it has) the aim of resetting to 0 the age of the
  %% queried value (and upstream chain). The removal of the final call
  %% to beta.find() nullifies such (alleged) aim.
  %%
  %% In order to restore such aim, a new instruction shall be designed
  %% and implemented for the cache, beta.pseudo_find(), whose aim is
  %% traversing the tree in search of the specified (x,k) and
  %% resetting beta.age (whatever it is) and gamma_j.age in all the
  %% chain until the position of the node is reached. If
  %% beta.data(x,k) is not defined, *no calc<w> node shall be
  %% created*. Probably, GC shall be triggered by this instruction
  %% too.
  Di = eval1(Xi, I, E, ice_sets:restrict_domain(K, D), [], W, T).

%%---------------------------------------------------------------------------------
%% Finding identifiers in the cache
%%---------------------------------------------------------------------------------
eval1(Xi, I, E, K, D, W, T) ->
  {D0, T0} = eval2(Xi, I, E, K, D, W, T),
  case ice_sets:is_k(D0) andalso ice_sets:subset(D0, ice_sets:domain(K)) of
    true ->
      case ice_sets:difference(D0, D) of
        [] ->
          {error, loop_detected, {already_known_dimensions, D0}};
        _ ->
          eval1(Xi, I, E, K, ice_sets:union(D, D0), W, T)
      end;
    false ->
      {D0, T0}
  end.

eval2(Xi, I, E, K, D, W, T) ->
  {D0, T0} = ice_cache:find(Xi, K, D, W, T),
  case D0 of
    {calc, W} ->
      case lists:keyfind(Xi, 1, E) of
        {_, E0} ->
          {D1, T1} = eval(E0, I, E, K, D, W, T0),
          ice_cache:add(Xi, K, D, W, T1, D1);
        false ->
          {error, undefined_identifier, Xi}
      end;
    {calc, _W1} ->
      eval2(Xi, I, E, K, D, W, T0 + 1);
    _ ->
      {D0, T0}
  end.

%%------------------------------------------------------------------------------
%% Internal - Functions which should be relocated to more suitable modules
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Lookup an ordinate for a given dimension identifier.
%%------------------------------------------------------------------------------
lookup_ordinate(_, []) ->
  undefined_dimension;
lookup_ordinate(D, [{D, Ordinate}|_]) ->
  Ordinate;
lookup_ordinate(D, [_|K]) ->
  lookup_ordinate(D, K).

%%------------------------------------------------------------------------------
%% @doc Fetch the odd elements of a list.
%%------------------------------------------------------------------------------
odd_elems(L) ->
  odd_elems(L, 0, []).

odd_elems([], _, Acc) ->
  lists:reverse(Acc);
odd_elems([X|L], N, Acc) when N rem 2 == 0 ->
  odd_elems(L, N+1, [X|Acc]);
odd_elems([_|L], N, Acc) ->
  odd_elems(L, N+1, Acc).

%%------------------------------------------------------------------------------
%% @doc Fetch the even elements of a list.
%%------------------------------------------------------------------------------
even_elems(L) ->
  even_elems(L, 0, []).

even_elems([], _, Acc) ->
  lists:reverse(Acc);
even_elems([X|L], N, Acc) when N rem 2 =/= 0 ->
  even_elems(L, N+1, [X|Acc]);
even_elems([_|L], N, Acc) ->
  even_elems(L, N+1, Acc).

%%------------------------------------------------------------------------------
%% @doc Freeze an abstractions context and return a new abstraction which 
%%      contains the frozen context.
%%------------------------------------------------------------------------------
freeze(Abs, I, E, K, D, W) ->
  Is = abs_intensions(Abs),
  Dis0 = eval_par(Is, I, E, K, D, W),
  case ice_sets:dim_union(Dis) of
    {true, Dims0} ->
      Dims0;
    {false, Dis1} ->
      case ice_sets:difference(Dis1, D) of
	[] ->
	  KD = ice_sets:restrict_domain(K, D),
	  FrozenK = ice_sets:restrict_domain(KD, Dis1),
	  freeze_abs(Abs, I, E, FrozenK);
	Dims1 ->
	  Dims1
      end
  end.

%%------------------------------------------------------------------------------
%% @doc Add the frozen intensions, environment and context to an abstraction.
%%------------------------------------------------------------------------------
freeze_abs({b_abs, _Is, Params, E0}, ClI, ClE, FrozenK) ->
  {b_abs, ClI, ClE, FrozenK, Params, E0};
freeze_abs({v_abs, _Is, Params, E0}, ClI, ClE, FrozenK) ->
  {v_abs, ClI, ClE, FrozenK, Params, E0};
freeze_abs({i_abs, _Is, E0}, FrozenK) ->
  {i_abs, ClI, ClE, FrozenK, E0}.

%%------------------------------------------------------------------------------
%% @doc Get the intensions within an abstraction (convenience function).
%%------------------------------------------------------------------------------
abs_intensions({b_abs, Is, _Params, _E0}) ->
  Is;
abs_intensions({v_abs, Is, _Params, _E0}) ->
  Is;
abs_intensions({i_abs, Is, _E0}) ->
  Is.





