%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2018 12:27
%%%-------------------------------------------------------------------
-module(pollution).
-author("jacob").

%% API
-export([createMonitor/0, addStation/3, addValue/5, testMe/0]).

-record(measurement, {time, type, value}).
-record(measurementStation, {name, position, measurements = []}).
-record(pollutionMonitor, {stations = []}).

%%% Create new stations monitor
createMonitor() -> #pollutionMonitor{stations = []}.

%%% Add station to existing monitor
addStation(Name, Position, Monitor = #pollutionMonitor{})
  -> case
       length(
         lists:filter(fun(#measurementStation{name = PMSName, position = PMSPosition, measurements = _}) ->
           (Name =/= PMSName) and (Position =/= PMSPosition) end, Monitor#pollutionMonitor.stations)
       ) == 0 of
       true -> addStation(#measurementStation{name = Name, position = Position}, Monitor);
       false -> erlang:error(stationAlreadyExists)
     end.

addStation(Station, Monitor) when is_record(Station, measurementStation) ->
  #pollutionMonitor{stations = [Station | Monitor#pollutionMonitor.stations]}.

%%% Add new measurement to station
addValue(Position, Time, Type, Value, Monitor) when is_tuple(Position) ->
  Found = getStationByPosition(Position, Monitor),
  #pollutionMonitor{
    stations = [(Monitor#pollutionMonitor.stations -- Found)
      ++ addValueToStation(Found, #measurement{time = Time, type = Type, value = Value})]
  };

addValue(Name, Time, Type, Value, Monitor) when is_list(Name) ->
  Found = getStationByName(Name, Monitor),
  #pollutionMonitor{
    stations = [(Monitor#pollutionMonitor.stations -- Found)
      ++ addValueToStation(Found, #measurement{time = Time, type = Type, value = Value})]
  }.

addValueToStation([Station | []], Measurement) when is_record(Station, measurementStation) and is_record(Measurement, measurement) ->
  #measurementStation{
    name = Station#measurementStation.name,
    position = Station#measurementStation.position,
    measurements = [Measurement | Station#measurementStation.measurements]
  }.

getStationByPosition(Position, Monitor) when is_tuple(Position) ->
  Found = [S || S <- Monitor#pollutionMonitor.stations, S#measurementStation.position =:= Position],
  checkFoundStationsLength(length(Found)),
  Found.

getStationByName(Name, Monitor) when is_list(Name) ->
  Found = [S || S <- Monitor#pollutionMonitor.stations, S#measurementStation.name =:= Name],
  checkFoundStationsLength(length(Found)),
  Found.

checkFoundStationsLength(FoundLen) when is_integer(FoundLen) ->
  case FoundLen == 0 of
    true -> throw("No station at this position was found.")
  end,
  case FoundLen /= 1 of
    true -> throw("There are a few stations at this position.")
  end,
  true.

%%% Remove measurement from station
removeValue(Position, Time, Type, Monitor) when is_tuple(Position) ->
  Found = getStationByPosition(Position),
  checkFoundStationsLength(length(Found)),
  FoundStations = [S || S <- Found, S#measurementStation]

testMe() ->
  P = createMonitor(),
  P1 = addStation("Aleja Słowackiego", {50.2345, 18.3445}, P),
  P2 = addValue({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1),
  P3 = addValue("Aleja Słowackiego", calendar:local_time(), "PM2,5", 113, P2),
  addValue("Nope", calendar:local_time(), "PM10", 300, P3).
