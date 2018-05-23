%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2018 08:46
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("jacob").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDeviation/2, crash/0, init/0, start_link/0, getMonitor/0]).

start() ->
  register(pollutionServer, spawn(?MODULE, init, [])).

start_link() ->
  register(pollutionServer, spawn_link(?MODULE, init, [])).

stop() ->
  pollutionServer ! {request, self(), stop},
  unregister(pollutionServer).

init() ->
  {ok, Monitor} = pollution:createMonitor(),
  loop(Monitor).

loop_default_request_handler(Pid, {State, Value}, Monitor) ->
  case State of
    ok ->
      Pid ! {response, {ok, Value}},
      loop(Value);
    _ ->
      Pid ! {response, {error, Value}},
      loop(Monitor)
  end.

loop(Monitor) ->
  receive
    {request, Pid, addStation, Args} ->
      {Name, Position} = Args,
      {State, Value} = pollution:addStation(Name, Position, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, addValue, Args} ->
      {NameOrPosition, Time, Type, Value} = Args,
      {State, Value} = pollution:addStation(NameOrPosition, Time, Type, Value, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, removeValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      {State, Value} = pollution:removeValue(NameOrPosition, Time, Type, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, getOneValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      {State, Value} = pollution:getOneValue(NameOrPosition, Time, Type, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, getStationMean, Args} ->
      {NameOrPosition, Type} = Args,
      {State, Value} = pollution:getStationMean(NameOrPosition, Type, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, getDailyMean, Args} ->
      {Type, Date} = Args,
      {State, Value} = pollution:getDailyMean(Type, Date, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, getDeviation, Args} ->
      {Type, Hour} = Args,
      {State, Value} = pollution:getDeviation(Type, Hour, Monitor),
      loop_default_request_handler(Pid, {State, Value}, Monitor);

    {request, Pid, getMonitor, _} ->
      loop_default_request_handler(Pid, {ok, Monitor}, Monitor);

    {request, Pid, stop} ->
      Pid ! {response, {ok, "Shutting down."}}

  end.

addStation(Name, Position) ->
  pollutionServer ! {request, self(), addStation, {Name, Position}}.

addValue(PositionOrName, Time, Type, Value) ->
  pollutionServer ! {request, self(), addValue, {PositionOrName, Time, Type, Value}}.

removeValue(PositionOrName, Time, Type) ->
  pollutionServer ! {request, self(), addValue, {PositionOrName, Time, Type}}.

getOneValue(PositionOrName, Time, Type) ->
  pollutionServer ! {request, self(), getOneValue, {PositionOrName, Time, Type}}.

getStationMean(PositionOrName, Type) ->
  pollutionServer ! {request, self(), getStationMean, {PositionOrName, Type}}.

getDailyMean(Type, Date) ->
  pollutionServer ! {request, self(), getDailyMean, {Type, Date}}.

getDeviation(Type, Hour) ->
  pollutionServer ! {request, self(), getDeviation, {Type, Hour}}.

getMonitor() ->
  pollutionServer ! {request, self(), getMonitor, {}}.

crash() ->
  pollutionServer ! {request, self(), addStation, {}}.