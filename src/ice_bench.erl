-module(ice_bench).

-export([time_f/1]).

time_f(Filename) ->
  T1 = erlang:now(),
  R = ice:f(Filename),
  T2 = erlang:now(),
  io:format("Time taken: ~p~n", [timer:now_diff(T2, T1) / 1000000]),
  R.


  
