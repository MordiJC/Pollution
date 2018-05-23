%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2018 17:14
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("jacob").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

addStationWithTheSameName_test() ->
  {ok, P} = pollution:createMonitor(),
  {ok, P1} = pollution:addStation("Station 1", {0, 0}, P),
  {State, _Msg} = pollution:addStation("Station 1", {1, 2}, P1),
  ?assertEqual(error, State).

addStationWithTheSamePosition_test() ->
  {_, P} = pollution:createMonitor(),
  {_, P1} = pollution:addStation("Station 1", {0, 0}, P),
  {State, _Msg} = pollution:addStation("Station 2", {0, 0}, P1),
  ?assertEqual(error, State).

getOneValueByPosition_test() ->
  Monitor = setupForValuesTesting(),
  ?assertEqual({ok, 150}, pollution:getOneValue(
    {0, 0},
    {{2018, 03, 12}, {12, 20, 00}},
    "PM10",
    Monitor)).

getOneValueByName_test() ->
  Monitor = setupForValuesTesting(),
  ?assertEqual({ok, 150}, pollution:getOneValue(
    "Station 1",
    {{2018, 03, 12}, {12, 20, 00}},
    "PM10",
    Monitor)).


addValuesFromList([], Monitor) -> Monitor;
addValuesFromList([H | T], Monitor) ->
  {NameOrPos, Time, Type, Value} = H,
  {ok, NMonitor} = pollution:addValue(
    NameOrPos, Time, Type, Value,
    addValuesFromList(T, Monitor)
  ),
  NMonitor.

setupForValuesTesting() ->
  {_, P} = pollution:createMonitor(),
  {_, P1} = pollution:addStation("Station 1", {0, 0}, P),
  {_, P2} = pollution:addStation("Station 2", {1, 1}, P1),
  {_, P3} = pollution:addStation("Station 3", {1, 0}, P2),
  P4 = addValuesFromList(
    [
      {"Station 1", {{2018, 03, 12}, {12, 20, 00}}, "PM10", 150},
      {"Station 1", {{2018, 03, 12}, {12, 40, 00}}, "PM10", 140},
      {"Station 1", {{2018, 03, 12}, {12, 20, 00}}, "PM2,5", 100},
      {"Station 1", {{2018, 03, 12}, {12, 40, 00}}, "PM2,5", 110},

      {"Station 2", {{2018, 03, 12}, {12, 20, 00}}, "PM10", 170},
      {"Station 2", {{2018, 03, 12}, {12, 40, 00}}, "PM10", 180},
      {"Station 2", {{2018, 03, 12}, {12, 20, 00}}, "PM2,5", 200},
      {"Station 2", {{2018, 03, 12}, {12, 40, 00}}, "PM2,5", 210},

      {"Station 3", {{2018, 03, 12}, {12, 20, 00}}, "PM10", 160},
      {"Station 3", {{2018, 03, 12}, {12, 40, 00}}, "PM10", 150},
      {"Station 3", {{2018, 03, 12}, {12, 20, 00}}, "PM2,5", 90},
      {"Station 3", {{2018, 03, 12}, {12, 40, 00}}, "PM2,5", 110}
    ],
    P3
  ),
  P4.