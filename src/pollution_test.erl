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

%%setup() ->
%%  Tm = {{2018, 03, 03},{12,34,56}},
%%  P = pollution:createMonitor(),
%%  P1 = pollution:addStation("Stacja 1", {50.2, 18.3}, P),
%%  P2 = pollution:addStation("Stacja 2", {50.3, 18.1}, P1).

addStationWithTheSameName_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Station 1", {0, 0}, P),
  ?assertThrow({error, _},
    pollution:addStation("Station 1", {1, 2}, P1)).

addStationWithTheSamePosition_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Station 1", {0, 0}, P),
  ?assertThrow({error, _},
    pollution:addStation("Station 2", {0, 0}, P1)).

getOneValueByPosition_test() ->
  Monitor = setupForValuesTesting(),
  ?assertEqual(150, pollution:getOneValue(
    {0,0},
    {{2018, 03, 12},{12, 20, 00}},
    "PM10",
    Monitor)).

getOneValueByName_test() ->
  Monitor = setupForValuesTesting(),
  ?assertEqual(150, pollution:getOneValue(
    "Station 1",
    {{2018, 03, 12},{12, 20, 00}},
    "PM10",
    Monitor)).


addValuesFromList([], Monitor) -> Monitor;
addValuesFromList([H|T], Monitor) ->
  {NameOrPos, Time, Type, Value} = H,
  pollution:addValue(
    NameOrPos, Time, Type, Value,
    addValuesFromList(T, Monitor)
  ).

setupForValuesTesting() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Station 1", {0, 0}, P),
  P2 = pollution:addStation("Station 2", {1, 1}, P1),
  P3 = pollution:addStation("Station 3", {1, 0}, P2),
  P4 = addValuesFromList(
    [
      {"Station 1", {{2018, 03, 12},{12, 20, 00}}, "PM10", 150},
      {"Station 1", {{2018, 03, 12},{12, 40, 00}}, "PM10", 140},
      {"Station 1", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 100},
      {"Station 1", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 110},

      {"Station 2", {{2018, 03, 12},{12, 20, 00}}, "PM10", 170},
      {"Station 2", {{2018, 03, 12},{12, 40, 00}}, "PM10", 180},
      {"Station 2", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 200},
      {"Station 2", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 210},

      {"Station 3", {{2018, 03, 12},{12, 20, 00}}, "PM10", 160},
      {"Station 3", {{2018, 03, 12},{12, 40, 00}}, "PM10", 150},
      {"Station 3", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 90},
      {"Station 3", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 110}
    ],
    P3
  ),
  P4.