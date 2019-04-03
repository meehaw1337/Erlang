%%%-------------------------------------------------------------------
%%% @author HP
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2019 10:43
%%%-------------------------------------------------------------------
-module(myLists).
-author("HP").

%% API
-export([contains/2, duplicateElements/1, sumFloats/2, rpn_util/2, rpn/2, rpn_start/1, parse/1, testRecord/0]).
-record(grupa, {nazwa, licznosc, stan=aktywna}).

testRecord() ->
  Grupa1 = #grupa{nazwa="Gruppa 1", licznosc=12},
  Grupa2 = #grupa{nazwa="Gruppa 2", licznosc=7, stan=0},
  io:format(Grupa1#grupa.nazwa).


contains([], X) ->
  false;
contains([H|T], H) ->
  true;
contains([H|T], X) ->
  contains(T, X).

duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).




sumFloats([], Acc) ->
  Acc;
sumFloats([H | T], Acc) when (is_float(H)) ->
  sumFloats(T, Acc+H).

parse(N) ->
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F,_} -> F
  end.

rpn("+", [Element1,Element2| T]) ->
  [Element2+Element1| T];
rpn("-", [Element1,Element2| T]) ->
  [Element2-Element1| T];
rpn("*", [Element1,Element2| T]) ->
  [Element2*Element1| T];
rpn("/", [Element1,Element2| T]) ->
  [Element2/Element1| T];
rpn("^", [Element1,Element2| T]) ->
  [math:pow(Element2,Element1)| T];
rpn("sqrt", [Element | T]) ->
  [math:sqrt(Element) | T];
rpn("sin", [Element | T]) ->
  [math:sin(Element) | T];
rpn("cos", [Element | T]) ->
  [math:cos(Element) | T];
rpn("tan", [Element | T]) ->
  [math:tan(Element) | T];
rpn(Element, Stack) ->
  [ parse(Element) | Stack ].

rpn_util([], Stack) ->
  io:format("~w \n", Stack);
rpn_util([Inputhead | Inputtail], Stack) ->
  rpn_util( Inputtail, rpn(Inputhead, Stack) ).

rpn_start(Lista) ->
  rpn_util(string:tokens(Lista, " "), []).








