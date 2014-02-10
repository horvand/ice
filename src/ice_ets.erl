-module(ice_ets).

-export([new/0, delete/0]).
-export([lookup/1, insert/2, insert_new/2]).

-define(TABLE_NAME, ets_cache).

%%------------------------------------------------------------------------------
%% @doc Create a new cache
%%------------------------------------------------------------------------------
new() ->
  ets:new(?TABLE_NAME, 
	  [named_table, public,
	   {read_concurrency, true}, 
	   {write_concurrency, true}]).

%%------------------------------------------------------------------------------
%% @doc Delete the cache
%%------------------------------------------------------------------------------
delete() ->
  ets:delete(?TABLE_NAME).

%%------------------------------------------------------------------------------
%% @doc Lookup an {Xi, K} pair in the cache
%%------------------------------------------------------------------------------
lookup({Xi, K} = XiK) ->
  case ets:lookup(?TABLE_NAME, XiK) of
    [] ->
      lookup({Xi, []}, K);
    [{_XiK, Vsn, {i, Dims} = V}] ->
      {Vsn, Dims};
    [{_XiK, Vsn, V}] ->
      {Vsn, V}
  end.

lookup({Xi, K} = XiK, KAcc) ->
  case ets:lookup(?TABLE_NAME, XiK) of
    [] ->
      [];
    [{_XiK, Vsn, {calc, _} = V}] ->
      {Vsn, V};
    [{_XiK, Vsn, {i, Dims}}] ->
      case ice_sets:restrict_domain(KAcc, Dims) of
	[] ->
	  {Vsn, Dims};
	Ki ->
	  lookup({Xi, lists:keysort(1, Ki)}, 
		 ice_sets:subtract_by_domain(KAcc, Dims))
      end;
    [{_XiK, Vsn, V}] ->
      {Vsn, V}
  end.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, K} into the cache.
%%------------------------------------------------------------------------------
insert({_Xi, _Vsn, _K} = XiK, Dims) when is_list(Dims) andalso length(Dims) > 0 ->
  insert(XiK, {i, lists:sort(Dims)});
insert({Xi, Vsn0, K} = XiK, V) ->
  case ets:lookup(?TABLE_NAME, {Xi, K}) of
    [{_, Vsn0, _}] ->
      ets:insert(?TABLE_NAME, {{XiK, V}, Vsn0, V});
    _Other ->
      false
  end.

%%------------------------------------------------------------------------------
%% @doc Insert a value at {Xi, K} into the cache if not already present.
%%------------------------------------------------------------------------------
insert_new({_Xi, _K} = XiK, {calc, _} = V) ->
  case ets:insert_new(?TABLE_NAME, {XiK, 0, V}) of
    true ->
      {true, V};
    false ->
      {false, V}
  end.
