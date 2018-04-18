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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getDeviation/3]).

-record(measurement, {time, type, value}).
-record(measurementStation, {name, position, measurements = []}).
-record(pollutionMonitor, {stations = []}).

%%% Create new stations monitor
createMonitor() -> #pollutionMonitor{stations = []}.

%%% Add station to existing monitor
addStation(Name, Position, Monitor = #pollutionMonitor{stations = Stations}) ->
  case lists:any(
    fun(#measurementStation{name = PMSName, position = PMSPosition}) ->
      (Name =:= PMSName) or (Position =:= PMSPosition)
    end,
    Stations
  ) of
    true -> throw({error, "Station already exists."});
    false -> addStationToMonitor(#measurementStation{name = Name, position = Position}, Monitor)
  end.

addStationToMonitor(Station, Monitor) when is_record(Station, measurementStation) ->
  #pollutionMonitor{stations = [Station | Monitor#pollutionMonitor.stations]}.


%%% Add new measurement to station
addValue(Position, Time, Type, Value, Monitor) when is_tuple(Position) and is_record(Monitor, pollutionMonitor) ->
  Found = getStationByPosition(Position, Monitor),
  #pollutionMonitor{
    stations = [addValueToStation(Found, #measurement{time = Time, type = Type, value = Value})]
    ++ (Monitor#pollutionMonitor.stations -- Found)
  };

addValue(Name, Time, Type, Value, Monitor) when is_list(Name) and is_record(Monitor, pollutionMonitor) ->
  Found = getStationByName(Name, Monitor),
  #pollutionMonitor{
    stations = [addValueToStation(Found, #measurement{time = Time, type = Type, value = Value})]
    ++ (Monitor#pollutionMonitor.stations -- Found)
  }.

addValueToStation([Station | []], Measurement) when is_record(Station, measurementStation) and is_record(Measurement, measurement) ->
  #measurementStation{
    name = Station#measurementStation.name,
    position = Station#measurementStation.position,
    measurements = [Measurement | Station#measurementStation.measurements]
  }.

getStationByPosition(Position, Monitor) when is_tuple(Position) ->
  Found = [S || S <- Monitor#pollutionMonitor.stations, S#measurementStation.position =:= Position],
  case length(Found) of
    0 -> throw("No station at this position was found.");
    1 -> true;
    _ -> throw("There are a few stations at this position.")
  end,
  Found.

getStationByName(Name, Monitor) when is_list(Name) ->
%%  Found = [S || S <- Monitor#pollutionMonitor.stations, S#measurementStation.name =:= Name],
  Found = lists:filter(
    fun(#measurementStation{name = SName}) ->
      (Name =:= SName)
    end,
    Monitor#pollutionMonitor.stations
  ),
  io:format("ABC ~p~n", [Found]),
  case length(Found) of
    0 -> throw("No station with this name was found.");
    1 -> true;
    _ -> throw("There are a few stations with this name.")
  end,
  Found.

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
getDailyMean(Type, Date, Monitor) ->
  {SumOfMeasurements, CountOfMeasurements} = lists:foldl(
    fun(#measurement{value = Value}, {Acc1, Acc2}) -> {Value + Acc1, Acc2 + 1} end,
    {0, 0},
    getMonitorMeasurementsByTypeAndDate(Type, Date, Monitor)
  ),
  case CountOfMeasurements == 0 of
    true -> 0;
    false -> (SumOfMeasurements / CountOfMeasurements)
  end.

getMonitorMeasurementsByTypeAndDate(Type, Date, #pollutionMonitor{stations = Stations}) ->
  lists:concat([getStationMeasurementsByTypeAndDate(Type, Date, S) || S <- Stations]).

getStationMeasurementsByTypeAndDate(Type, Date, #measurementStation{measurements = Measurements}) ->
  lists:filter(
    fun(#measurement{type = MType, time = {MDate, _}}) ->
      (MType =:= Type) and (Date =:= MDate)
    end,
    Measurements
  ).

%%% Calculate standard deviation of measurements by type and hour
getDeviation(Type, Hour, Monitor) ->
  Measurements = getMonitorMeasurementsByTypeAndHour(Type, Hour, Monitor),
  {SumOfMeasurements, CountOfMeasurements} = lists:foldl(
    fun(#measurement{value = Value}, {Sum, Cnt}) -> {Value + Sum, 1 + Cnt} end,
    {0, 0},
    Measurements
  ),
  Avg = if
          CountOfMeasurements == 0 -> 0;
          true -> SumOfMeasurements / CountOfMeasurements
        end,
  VarianceDenominator = lists:foldl(
    fun(#measurement{value = Value}, VD) -> (VD + math:pow(Value - Avg, 2)) end,
    0,
    Measurements
  ),
  if
    CountOfMeasurements == 0 -> 0;
    true -> math:sqrt(VarianceDenominator / CountOfMeasurements)
  end.

getMonitorMeasurementsByTypeAndHour(Type, Hour, #pollutionMonitor{stations = Stations}) ->
  lists:concat([getStationMeasurementsByTypeAndHour(Type, Hour, S) || S <- Stations]).

getStationMeasurementsByTypeAndHour(Type, Hour, #measurementStation{measurements = Measurements}) ->
  lists:filter(
    fun(#measurement{type = MType, time = {_, {MHour, _, _}}}) ->
      (MType =:= Type) and (Hour =:= MHour)
    end,
    Measurements
  ).