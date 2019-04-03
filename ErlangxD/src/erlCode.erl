%%%-------------------------------------------------------------------
%%% @author HP
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. mar 2019 16:37
%%%-------------------------------------------------------------------
-module(erlCode).
-author("HP").

%% API
-export([power/2]).

power(Num, 1) ->
  Num;
power(Num, Pow) ->
  Num * power(Num, Pow - 1).


