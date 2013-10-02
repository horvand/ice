%% See LICENSE for licensing information.
%% -*- coding: utf-8 -*-
-module(tv).
-behaviour(gen_server).

%% tv: Tea Visualiser.

-export([hook/3]).

-export([init/1, terminate/2, code_change/3, start_link/0,
         handle_info/2, handle_call/3, handle_cast/2]).

-define(tv, isee).

%% API

-type name() :: string() | atom().

-spec hook (Module::name(), Descr::name(), Data::term()) -> any().
%% Just call it like so: `tv:hook(?MODULE, "caching var", {D,A,Ta})`.
hook (Name, Descr, Data) ->
    gen_server:cast(?MODULE, {pass, Name, ?tv:time(), Descr, Data}).

start_link () ->
    R = gen_server:start_link({local,?MODULE}, ?MODULE, [], []),
    io:format("Server at ~p\n",[R]),
    R.

%% Internals

handle_cast (M={pass, Name, Time, Descr, Data}, S0) ->
    _ = case whereis(?tv) of
        _P when is_pid(_P) ->
            timer:sleep(100),
            io:format("PAAAASSing ~p\n",[M]),
            ?tv:pass(Name, Time, Descr, Data);
        _ ->
            {error, {unable_to_pass,server_down}}
    end,
    {noreply, S0};
handle_cast (_Request, S0) ->
    {noreply, S0}.

init (_) -> {ok, []}.

terminate (Reason, S0) ->
    io:format("\nTerminating with ~p.\n", [[{reason,Reason}, {state,S0}]]).

code_change (_OldVsn, S0, _Extra) -> {ok, S0}.

handle_info (_Msg, S0) -> {noreply, S0}.

handle_call (_Request, _From, S0) -> {noreply, S0}.

%% End of Module.
