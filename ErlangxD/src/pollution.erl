%%%-------------------------------------------------------------------
%%% @author Michal Komar
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. kwi 2019 14:37
%%%-------------------------------------------------------------------
-module(pollution).
-author("Michal Komar").

%% API
-export([createMonitor/0, getStation/2, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getAllValues/1, getMovingMean/4]).


%% Records
-record(station, {name, coords}).
-record(value, {date, type, measurement_value}).

createMonitor() ->
  #{}.

getStation({X, Y}, Monitor) ->
  Stations = maps:keys(Monitor),

  Result = [ResultList || ResultList <- Stations, (ResultList#station.coords == {X,Y})],

  case Result of
    [] -> station_not_found;
    _ -> [Res] = Result, % wyciagniecie z listy
      Res
  end;
getStation(StationData, Monitor) ->
  Stations = maps:keys(Monitor),

  Result = [ResultList || ResultList <- Stations, string:equal(ResultList#station.name, StationData)],

  case Result of
    [] -> station_not_found;
    _ -> [Res] = Result, % wyciagniecie z listy
      Res
  end.

addStation(StationName, {X, Y}, Monitor) ->
  Station1 = getStation(StationName, Monitor),

  case Station1 of
    station_not_found ->
      Station2 = getStation(StationName, Monitor),
      case Station2 of
        station_not_found -> maps:put(#station{name=StationName, coords={X, Y}}, [], Monitor);
        _ -> {error,"Station with these coordinates already exists in the monitor."}
      end;
    _ -> {error,"Station with that name already exists in the monitor."}
  end.


addValue(StationData, Date, Type, Value, Monitor) ->
  Station = getStation(StationData, Monitor),

  case Station of
    station_not_found -> {error, "Station with that name or those coordinates does not exist in the monitor"};
    _ -> Values = maps:get(Station, Monitor),
      DateAndTypeChecker = fun (Val) -> (Val#value.date == Date) and string:equal(Val#value.type, Type) end,
      case lists:any(DateAndTypeChecker, Values) of
        true -> {error, "This measurement already exists"};
        false -> maps:put(Station, [ #value{date=Date, type=Type, measurement_value=Value} | Values ], Monitor)
      end
  end.

removeValue(StationData, Date, Type, Monitor) ->
  Station = getStation(StationData, Monitor),

  case Station of
    station_not_found -> {error, "Station with that name or those coordinates does not exist in the monitor"};
    _ -> Values = maps:get(Station, Monitor),
      DateAndTypeChecker = fun (Val) -> (Val#value.date == Date) and string:equal(Val#value.type, Type) end,
      case lists:any(DateAndTypeChecker, Values) of
        false -> {error, "This value does not exist"};
        true -> maps:put(Station, [NewValues || NewValues <- Values, DateAndTypeChecker(NewValues)==false], Monitor)
      end
  end.

getOneValue(StationData, Date, Type, Monitor) ->
  Station = getStation(StationData, Monitor),

  case Station of
    station_not_found -> {error, "Station with that name or those coordinates does not exist in the monitor"};
    _ -> Values = maps:get(Station, Monitor),
      MatchingValue = [ ValuesTmp || ValuesTmp <- Values, (ValuesTmp#value.date == Date) and string:equal(ValuesTmp#value.type, Type)],
      case MatchingValue of
        [] -> {error, "This value does not exist"};
        _ -> [Result] = MatchingValue,
          Result#value.measurement_value
      end
  end.


getStationMean(StationData, Type, Monitor) ->
  Station = getStation(StationData, Monitor),

  case Station of
    station_not_found -> {error, "Station with that name or those coordinates does not exist in the monitor"};
    _ -> Values = maps:get(Station, Monitor),
      MatchingValues = [ ValuesTmp || ValuesTmp <- Values, string:equal(ValuesTmp#value.type, Type)],
      {Sum, NumberOfValues} = lists:foldl( fun (Value, {S, N}) -> {S + Value#value.measurement_value, N+1} end, {0,0}, MatchingValues),
      case NumberOfValues of
        0 -> {error, "No matching values found"};
        _ -> Sum / NumberOfValues
      end
  end.

getDay(Date) ->
  {Day, _} = Date,
  Day.

getDailyMean(Type, Day, Monitor) ->
  AverageFun = fun (_, Value, {Sum, NumberOfValues}) ->
    MatchingValues = [Values || Values <- Value, ( getDay(Values#value.date )==Day ) and string:equal(Values#value.type, Type)],
    lists:foldl( fun (Value, {S, N}) -> {S + Value#value.measurement_value, N+1} end, {Sum, NumberOfValues}, MatchingValues)
               end,
  {Sum, NumberOfValues} = maps:fold(AverageFun, {0,0}, Monitor),
  case NumberOfValues of
    0 -> {error, "No matching values found"};
    _ -> Sum / NumberOfValues
  end.


getAllValues(Monitor) ->
  maps:values(Monitor).

movingMeanUtil(StationData, Date, Type, Monitor, Number) ->
  Value = getOneValue(StationData, Date, Type, Monitor),
  {{Y, M, D},{H, Mn, S}} = Date,
  NewDate = {{Y, M, D}, {H-1, Mn, S}},

  case Number of
    0 -> 0;
    _ ->
    case Value of
      {error, "This value does not exist"} -> movingMeanUtil(StationData, NewDate, Type, Monitor, Number-1);
      _ -> Value * Number + movingMeanUtil(StationData, NewDate, Type, Monitor, Number-1)
    end
  end.


getMovingMean(StationData, Date, Type, Monitor) ->
  Station = getStation(StationData, Monitor),
  case Station of
    station_not_found -> {error, "Station with those coordinates does not exist"};
    _ -> movingMeanUtil(StationData, Date, Type, Monitor, 24)
  end.



