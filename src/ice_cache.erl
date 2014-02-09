-module(ice_cache).

%% ice_cache interface exports
-export([create/0, delete/0]).
-export([find/4, add/5]).

-define(FMT_D(V), %% Format missing dimensions
        case V of
          Dims when is_list(Dims) andalso length(Dims) > 0 ->
            "{i, ~p}";
          _ ->
            "~p"
        end).

%%------------------------------------------------------------------------------
%% @doc Create a named ets table which represents the cache
%%------------------------------------------------------------------------------
create() ->
  ice_dtree:new().

%%------------------------------------------------------------------------------
%% @doc Delete the named ets table which represents the cache
%%------------------------------------------------------------------------------
delete() ->
  ice_dtree:delete().

%%------------------------------------------------------------------------------
%% @doc Find an identifier with a specific context K restricted by the domain D
%%------------------------------------------------------------------------------
find(X, K, D, {Id0, _} = W0) ->
  KD = lists:keysort(1, ice_sets:restrict_domain(K, D)),
  case ice_dtree:insert_new({X,KD}, {calc,W0}) of
    {true, {calc,W0}} ->
      {calc,W0};
    {false, {calc, {Id1,_} = W1} = V} ->
      case lists:prefix(Id1, Id0) of
        true ->
          V;
        false ->
          V
      end;
    {false, V} ->
      V
  end.

%%------------------------------------------------------------------------------
%% @doc Add an {identifier, context, value} to the cache
%%------------------------------------------------------------------------------
add(X, K, D, W, V) ->
  KD = lists:keysort(1, ice_sets:restrict_domain(K, D)),
  case ice_dtree:lookup({X,KD}) of
    {calc, W} ->
      true = ice_dtree:insert({X,KD}, V),
      V;
    _ ->
      %%hang
      V
  end.
