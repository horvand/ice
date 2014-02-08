-module(ice_sets).

-export([intersection/2, difference/2]).
-export([restrict/2, union/2, restrict_domain/2]).
-export([subset/2, domain/1, is_known_dimension/1, is_context/1]).
-export([subtract/2, subtract_domain/2, subtract_by_domain/2]).
-export([perturb/2, dim_union/1]).
-export([identical/2]).

%%-------------------------------------------------------------------------------------
%% @doc The intersection of sets A and B
%%-------------------------------------------------------------------------------------
intersection(A, B) ->
  sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).

%%------------------------------------------------------------------------------
%% @doc The difference between set A and set B
%%------------------------------------------------------------------------------
difference(A, B) ->
  sets:to_list(sets:subtract(sets:from_list(A), sets:from_list(B))).

%%-------------------------------------------------------------------------------------
%% @doc Restrict ( <| ) set A by set B
%%-------------------------------------------------------------------------------------
restrict(A, B) ->
  [X || X <- A, lists:member(X, B)].

%%-------------------------------------------------------------------------------------
%% @doc Restrict ( <| ) set A by domain B
%%-------------------------------------------------------------------------------------
restrict_domain(A, Domain) ->
  [{X, V} || {X, V} <- A, lists:member(X, Domain)].

%%-------------------------------------------------------------------------------------
%% @doc Subtract a set A from set B
%%-------------------------------------------------------------------------------------
subtract(A, B) ->
  [X || X <- A, lists:member(X, B) =:= false].

%%------------------------------------------------------------------------------
%% @doc Subtract the domain B from the domain of A
%%------------------------------------------------------------------------------
subtract_domain(A, B) ->
  [{X,V} || {X,V} <- A, lists:member(X, B) =:= false].

%%------------------------------------------------------------------------------
%% @doc Subtract the domain of B from the domain of A
%%------------------------------------------------------------------------------
subtract_by_domain(A, B) ->
  [{X,V} || {X,V} <- A, lists:keymember(X, 1, B) =:= false].

%%-------------------------------------------------------------------------------------
%% @doc Perturb (|) set A by set B
%%-------------------------------------------------------------------------------------
perturb(A, B) ->
  A1 = subtract_by_domain(A, B),
  union(A1, B).

%%-------------------------------------------------------------------------------------
%% @doc The union of sets A and B
%%-------------------------------------------------------------------------------------
union(A, B) ->
  sets:to_list(sets:union(sets:from_list(A), sets:from_list(B))).

%%-------------------------------------------------------------------------------------
%% @doc The union of sets in Dis where if Di is a set of known dimensions, Ui MaxI
%%-------------------------------------------------------------------------------------
dim_union(Dis) ->
  case lists:any(fun ice_sets:is_k/1, Dis) of
    true ->
      {true, dim_union(Dis, [])};
    false ->
      {false, Dis}
  end.

dim_union([], UDis) ->
  UDis;
dim_union([Di|Dis], UDis) ->
  case is_context(Di) of
    true ->
      dim_union(Dis, union(Di, UDis));
    false ->
      dim_union(Dis, UDis)
  end.

%%-------------------------------------------------------------------------------------
%% @doc Is A a subset of B?
%%-------------------------------------------------------------------------------------
subset(A, B) ->
  sets:is_subset(sets:from_list(A), sets:from_list(B)).

%%-------------------------------------------------------------------------------------
%% @doc Domain of set A
%%-------------------------------------------------------------------------------------
domain(A) ->
  [K || {K, _} <- A].

%%-------------------------------------------------------------------------------------
%% @doc Are two sets identical
%%-------------------------------------------------------------------------------------
identical([],[]) -> 
  true;
identical(A,B) ->
  (length(A) == length(B)) andalso
    (length([true || X <- A, lists:member(X, B)]) == length(A)).

%%-------------------------------------------------------------------------------------
%% @doc Check whether A is a set of known dimensions
%%-------------------------------------------------------------------------------------
is_known_dimension({dim, {_Pos,_Idx}, A}) when is_list(A) orelse is_atom(A) ->
  true;
is_known_dimension({phi, A}) when is_list(A) orelse is_atom(A) ->
  true;
is_known_dimension(_) ->
  false.

is_context(A) when is_list(A) ->
  %% Use of any here since any 'known dimension' within a context means that there
  %% is possibly a missing dimension ? Verify semantics of this form
  lists:any(fun (X) -> is_known_dimension(X) end, A);
is_context(_) ->
  false.
