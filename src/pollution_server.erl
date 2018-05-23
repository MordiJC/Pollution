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
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDeviation/2, crash/0, init/0, start_link/0]).

start() ->
  register(pollutionServer, spawn(?MODULE, init, [])).

start_link() ->
  register(pollutionServer, spawn_link(?MODULE, init, [])).

stop() ->
  pollutionServer ! {request, self(), stop},
  unregister(pollutionServer).

init() ->
  loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    {request, Pid, addStation, Args} ->
      {Name, Position} = Args,
      try pollution:addStation(Name, Position, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, addValue, Args} ->
      {NameOrPosition, Time, Type, Value} = Args,
      try pollution:addStation(NameOrPosition, Time, Type, Value, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, removeValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      try pollution:removeValue(NameOrPosition, Time, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, getOneValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      try pollution:getOneValue(NameOrPosition, Time, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, getStationMean, Args} ->
      {NameOrPosition, Type} = Args,
      try pollution:getStationMean(NameOrPosition, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, getDailyMean, Args} ->
      {Type, Date} = Args,
      try pollution:getDailyMean(Type, Date, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

    {request, Pid, getDeviation, Args} ->
      {Type, Hour} = Args,
      try pollution:getDeviation(Type, Hour, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} ->
          Pid ! {response, {error, Reason}},
          loop(Monitor)
      end;

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

crash() ->
  pollutionServer ! {request, self(), addStation, {}}.