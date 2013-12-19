-module(tcache).

-behaviour(gen_server).

%% tcache API exports
-export([find/5, add/6, collect/0, stop/0]).

%% gen_server API exports
-export([start_link/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, init/1, terminate/2]).

-define(TABLE_NAME, ice_cache).

%%------------------------------------------------------------------------------
%% State
%%------------------------------------------------------------------------------
-record(state, { 
	  ck    = 0,
	  age   = 2,
	  data  = tdtree:new(?TABLE_NAME),
	  limit = undefined
	 }).

%%------------------------------------------------------------------------------
%% gen_server API implementation
%%------------------------------------------------------------------------------
start_link(Limit) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Limit], []).

init([Limit]) ->
  {ok, #state{ limit = Limit }}.

terminate(_, S) ->
  ok.

%%------------------------------------------------------------------------------
%% tcache API
%%------------------------------------------------------------------------------
find(X, K, D, W, T) ->
  gen_server:call(?MODULE, {find, X, K, D, W, T}).

add(X, K, D, W, T, V) ->
  gen_server:call(?MODULE, {add, X, K, D, W, T, V}).

collect() ->
  gen_server:call(?MODULE, collect).

stop() ->
  gen_server:call(?MODULE, terminate).

%%------------------------------------------------------------------------------
%% API implementation
%%------------------------------------------------------------------------------
handle_call({find, X, K, D, W0, T}, _From, S0) when T > S0#state.ck ->
  S1 = S0#state{ ck = T }, %% advance B.ck to t
  find_update(X, K, D, W0, T, S1);
handle_call({find, X, K, D, W0, T}, _From, S0) ->
  find_update(X, K, D, W0, T, S0);

handle_call({add, X, K, D, W, T, V}, _From, S0) when T > S0#state.ck ->
  S1 = S0#state{ ck = T }, 
  add_update(X, K, D, W, T, V, S1);
handle_call({add, X, K, D, W, T, V}, _From, S0) ->
  add_update(X, K, D, W, T, V, S0);

handle_call(collect, _From, S0) ->
  {reply, ok, S0};

handle_call(terminate, _From, S0) ->
  {stop, normal, ok, S0}.

%%------------------------------------------------------------------------------
%% internal
%%------------------------------------------------------------------------------
find_update(X, K, D, {Id0,_}=W0, _T, S0) ->
  KD = lists:keysort(1, tset:restrict_domain(K, D)),
  case tdtree:lookup({X,KD}, S0#state.data) of
    [] ->
      Tr = tdtree:insert({X,KD,{calc,W0}}, S0#state.data),
      S2 = S0#state{ck = S0#state.ck + 1},
      {reply, {{calc,W0}, S2#state.ck},  S2};
    {calc, {Id1,_}=W1} = V ->
      case lists:prefix(Id1, Id0) of
	true ->
	  %% FIXME
	  %% Threads can be <= to others which is wrong, but it works...
	  %% io:format("Thread ~p is less than or equal to ~p~n", [W1, W0]),
	  {reply, hang, S0};
	false ->
	  {reply, {V, S0#state.ck}, S0}
      end;
    V ->
      {reply, {V, S0#state.ck}, S0}
  end.

add_update(X, K, D, W, _T, V1, S0) ->
  KD = lists:keysort(1, tset:restrict_domain(K, D)),
  case tdtree:lookup({X,KD}, S0#state.data) of
    [] ->
      {reply, hang, S0};
    {calc, W} ->
      case V1 of
	V1 when is_list(V1) ->
	  io:format("Inserting {~p,~p} = {i,~p,[]}~n", [X,KD,V1]),
	  Tr = tdtree:insert({X,KD,{i,V1,[]}}, S0#state.data);
	V1 ->
	  io:format("Inserting {~p,~p} = ~p~n", [X,KD,V1]),
	  Tr = tdtree:insert({X,KD,V1}, S0#state.data)
      end,
      S1 = S0#state{ck = S0#state.ck + 1},
      {reply, {V1, S1#state.ck}, S1};
    {calc, _} = Thr ->
      io:format("Wrong thread ~p~n", [Thr]),
      {reply, hang, S0};
    Other ->
      io:format("Other = ~p, W = ~p~nData1 = ~p~n", [Other, W, S0#state.data]),
      {reply, hang, S0}
  end.

%%------------------------------------------------------------------------------
%% Not implemented
%%------------------------------------------------------------------------------
code_change(_, _, _) ->
  not_implemented.

handle_cast(_, _) ->
  not_implemented.

handle_info(_, _) ->
  not_implemented.
