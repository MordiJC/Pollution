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
-export([start/0, stop/0]).

start() ->
  register(pollutionServer, spawn(pollution_server, init, [pollution:createMonitor()])).

stop() ->
  unregister(pollutionServer).


init(Monitor) ->
  loop(Monitor).

loop(Monitor) ->
  receive
    {request, Pid, addStation, Args} ->
      {Name, Position} = Args,
      try pollution:addStation(Name, Position, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, addValue, Args} ->
      {NameOrPosition, Time, Type, Value} = Args,
      try pollution:addStation(NameOrPosition, Time, Type, Value, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, removeValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      try pollution:removeValue(NameOrPosition, Time, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, getOneValue, Args} ->
      {NameOrPosition, Time, Type} = Args,
      try pollution:getOneValue(NameOrPosition, Time, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, getStationMean, Args} ->
      {NameOrPosition, Type} = Args,
      try pollution:getStationMean(NameOrPosition, Type, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, getDailyMean, Args} ->
      {Type, Date} = Args,
      try pollution:getDailyMean(Type, Date, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, getDeviation, Args} ->
      {Type, Hour} = Args,
      try pollution:getDeviation(Type, Hour, Monitor) of
        M ->
          Pid ! {response, {ok, ""}},
          loop(M)
      catch
        throw:{error, Reason} -> Pid ! {response, {error, Reason}}
      end;

    {request, Pid, stop} ->
      Pid ! {response, {ok, "Shutting down."}}

  end.