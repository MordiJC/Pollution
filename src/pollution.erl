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
-export([createMonitor/0, addStation/3, addValue/5, testMe/0, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

-record(measurement, {time, type, value}).
-record(measurementStation, {name, position, measurements = []}).
-record(pollutionMonitor, {stations = []}).

%%% Create new stations monitor
createMonitor() -> #pollutionMonitor{stations = []}.

%%% Add station to existing monitor
addStation(Name, Position, Monitor) when is_record(Monitor, pollutionMonitor)
  -> case
       length(
         lists:filter(fun(#measurementStation{name = PMSName, position = PMSPosition}) ->
           (Name =/= PMSName) and (Position =/= PMSPosition) end, Monitor#pollutionMonitor.stations)
       ) == 0 of
       true -> addStationToMonitor(#measurementStation{name = Name, position = Position}, Monitor);
       false -> throw("Station already exists.")
     end.

addStationToMonitor(Station, Monitor) when is_record(Station, measurementStation) ->
  #pollutionMonitor{stations = [Station | Monitor#pollutionMonitor.stations]}.


%%% Add new measurement to station
addValue(Position, Time, Type, Value, Monitor) when is_tuple(Position) and is_record(Monitor, pollutionMonitor) ->
  Found = getStationByPosition(Position, Monitor),
  #pollutionMonitor{
    stations = [(Monitor#pollutionMonitor.stations -- Found)
      ++ addValueToStation(Found, #measurement{time = Time, type = Type, value = Value})]
  };

addValue(Name, Time, Type, Value, Monitor) when is_list(Name) and is_record(Monitor, pollutionMonitor) ->
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
  case FoundLen of
    0 -> throw("No station at this position was found.");
    1 -> true;
    _ -> throw("There are a few stations at this position.")
  end.


%%% Remove measurement from station
removeValue(Position, Time, Type, Monitor) when is_tuple(Position) and is_record(Monitor, pollutionMonitor) ->
  removeValueFromStations(getStationByPosition(Position, Monitor), Time, Type, Monitor);

removeValue(Name, Time, Type, Monitor) when is_list(Name) and is_record(Monitor, pollutionMonitor) ->
  removeValueFromStations(getStationByName(Name, Monitor), Time, Type, Monitor).

removeValueFromStations(Stations, Time, Type, Monitor) ->
  FoundStations = [S || S <- Stations, length(filterMeasurements(Time, Type, S#measurementStation.measurements)) > 0],
  case length(FoundStations) of
    0 -> throw("No stations with required measurements found.");
    1 -> #pollutionMonitor{
      stations = [removeValueFromStation(Time, Type, hd(FoundStations)) | (Monitor#pollutionMonitor.stations -- FoundStations)]
    };
    _ -> throw("Too many stations found.")
  end.

removeValueFromStation(Time, Type, Station) ->
  #measurementStation{
    name = Station#measurementStation.name,
    position = Station#measurementStation.position,
    measurements = (Station#measurementStation.measurements -- filterMeasurements(Time, Type, Station#measurementStation.measurements))}.

filterMeasurements(Time, Type, Measurements) when is_list(Measurements) ->
  lists:filter(
    fun(#measurement{time = Tm, type = Tp}) -> (Tm =:= Time) and (Tp =:= Type) end,
    Measurements
  ).

%%% Get measurement from station
getOneValue(Position, Time, Type, Monitor) when is_tuple(Position) and is_record(Monitor, pollutionMonitor) ->
  getOneValue(Time, Type, getStationByPosition(Position, Monitor));
getOneValue(Name, Time, Type, Monitor) when is_list(Name) and is_record(Monitor, pollutionMonitor) ->
  getOneValue(Time, Type, getStationByName(Name, Monitor)).

getOneValue(Time, Type, Stations) ->
  case length(Stations) of
    0 -> throw("No stations with required measurements found.");
    1 -> (hd(
      filterMeasurements(Time, Type, (hd(Stations))#measurementStation.measurements)
    ))#measurement.value;
    _ -> throw("Too many stations found.")
  end.

%%% Get station mean value
getStationMean(Position, Type, Monitor) when is_tuple(Position) and is_record(Monitor, pollutionMonitor) ->
  getMean((hd(getStationByPosition(Position, Monitor)))#measurementStation.measurements, Type);
getStationMean(Name, Type, Monitor) when is_list(Name) and is_record(Monitor, pollutionMonitor) ->
  getMean((hd(getStationByName(Name, Monitor)))#measurementStation.measurements, Type).

getMean([], _) -> 0;
getMean(Measurements, Type) ->
  SumFun = fun(#measurement{value = Value}, {Acc, Cnt}) -> {Value + Acc, Cnt + 1} end,
  FilteredMeasurements = lists:filter(
    fun(#measurement{type = Tp}) -> Tp =:= Type end,
    Measurements
  ),
  {MeasurementsSum, MeasurementsCount} = lists:foldl(SumFun, {0, 0}, FilteredMeasurements),
  MeasurementsSum / MeasurementsCount.

%%% Get daily mean value
%%% TODO: FIX THIS!!!
getDailyMean(Type, Date = {_, _, _}, Monitor) when is_record(Monitor, pollutionMonitor) ->
  MeasurementsPredicate = fun(#measurement{type = Tp, time = {Dt, _}}) ->
    (Tp =:= Type) and (Dt =:= Date) end,
  StationsPredicate =
    fun(#measurementStation{measurements = Measurements}) ->
      lists:any(
        MeasurementsPredicate,
        Measurements
      )
    end,
  MeasurementsSumFun = fun(#measurement{value = Value}, {Acc, Cnt}) -> {Value + Acc, Cnt + 1} end,
  {SumOfMeasurements, CountOfMeasurements} = lists:foldl(
    fun({S, C}, {AcuS, AcuC}) ->
      ({S + AcuS, C + AcuC})
    end,
    {0, 0},
    lists:map(
      fun(#measurementStation{measurements = Meas}) ->
        lists:foldl(MeasurementsSumFun, {0, 0}, Meas)
      end,
      lists:filter(
        StationsPredicate,
        Monitor#pollutionMonitor.stations
      )
    )
  ),
  case CountOfMeasurements == 0 of
    true -> 0;
    false -> (SumOfMeasurements / CountOfMeasurements)
  end.

testMe() ->
  Tm = calendar:local_time(),
  P = createMonitor(),
  P1 = addStation("Aleja Słowackiego", {50.2345, 18.3445}, P),
  P2 = addValue({50.2345, 18.3445}, Tm, "PM10", 59, P1),
  P3 = addValue("Aleja Słowackiego", Tm, "PM2,5", 113, P2),
  io:format("P3:~n~p~n", [P3]),
  io:format("\"Aleja Słowackiego\" PM2,5 = ~p~n", [getOneValue("Aleja Słowackiego", Tm, "PM2,5", P3)]),
  io:format("~p PM10 = ~p~n", [{50.2345, 18.3445}, getOneValue({50.2345, 18.3445}, Tm, "PM10", P3)]),
  io:format("\"Aleja Słowackiego\" Mean PM2,5 = ~p~n", [getStationMean("Aleja Słowackiego", "PM2,5", P3)]),
  io:format("~p Mean PM10 = ~p~n", [{50.2345, 18.3445}, getStationMean({50.2345, 18.3445}, "PM10", P3)]),
  io:format("Daily mean for PM10:~n~p~n", [getDailyMean("PM10", lists:nth(2, tuple_to_list(Tm)), P3)]),
  P4 = removeValue({50.2345, 18.3445}, Tm, "PM10", P3),
  P5 = removeValue("Aleja Słowackiego", Tm, "PM2,5", P4),
  io:format("P5:~n~p~n", [P5]).
