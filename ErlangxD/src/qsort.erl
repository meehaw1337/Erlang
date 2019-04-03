%%%-------------------------------------------------------------------
%%% @author HP
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2019 09:54
%%%-------------------------------------------------------------------
-module(qsort).
-author("HP").

%% API
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) ->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg ].

qs([]) ->
  [];
qs([Pivot|Tail]) ->
  qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).

randomElems(0, _, _) ->
  [];
randomElems(N, Min, Max) ->
  case Min >= Max of
    false -> [Min - 1 + rand:uniform(Max-Min + 1) || X <- lists:seq(1, N)];
    true -> io:format("Minimum greater than maximum")
  end.

compareSpeeds(List, Fun1, Fun2) ->
  io:format("Fun1: ~w ~n", [timer:tc(Fun1, [List])]),
  io:format("Fun1: ~w ~n", [timer:tc(Fun2, [List])]).

