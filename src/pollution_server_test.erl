%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 12:08
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("jacob").

%% API
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

startServer() ->
  pollution_server:start().

stopServer() ->
  pollution_server:stop().

handleResponse() ->
  receive
    {response, {error, _}} -> false;
    {response, {ok, _}} -> true
  end.

addStationWithTheSameName_test() ->
  startServer(),
  pollution_server:addStation("Station 1", {0, 0}),
  handleResponse(),
  pollution_server:addStation("Station 1", {1, 2}),
  ?assert(handleResponse() =:= false),
  stopServer().

addStationWithTheSamePosition_test() ->
  startServer(),
  pollution_server:addStation("Station 1", {0, 0}),
  handleResponse(),
  pollution_server:addStation("Station 2", {0, 0}),
  ?assert(handleResponse() =:= false),
  stopServer().

%%getOneValueByPosition_test() ->
%%  Monitor = setupForValuesTesting(),
%%  ?assertEqual(150, pollution_server:getOneValue(
%%    {0,0},
%%    {{2018, 03, 12},{12, 20, 00}},
%%    "PM10")).
%%
%%getOneValueByName_test() ->
%%  Monitor = setupForValuesTesting(),
%%  ?assertEqual(150, pollution_server:getOneValue(
%%    "Station 1",
%%    {{2018, 03, 12},{12, 20, 00}},
%%    "PM10",
%%    Monitor)).
%%
%%
%%addValuesFromList([], Monitor) -> Monitor;
%%addValuesFromList([H|T], Monitor) ->
%%  {NameOrPos, Time, Type, Value} = H,
%%  pollution_server:addValue(
%%    NameOrPos, Time, Type, Value,
%%    addValuesFromList(T, Monitor)
%%  ).
%%
%%setupForValuesTesting() ->
%%  P = pollution_server:createMonitor(),
%%  P1 = pollution_server:addStation("Station 1", {0, 0}, P),
%%  P2 = pollution_server:addStation("Station 2", {1, 1}, P1),
%%  P3 = pollution_server:addStation("Station 3", {1, 0}, P2),
%%  P4 = addValuesFromList(
%%    [
%%      {"Station 1", {{2018, 03, 12},{12, 20, 00}}, "PM10", 150},
%%      {"Station 1", {{2018, 03, 12},{12, 40, 00}}, "PM10", 140},
%%      {"Station 1", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 100},
%%      {"Station 1", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 110},
%%
%%      {"Station 2", {{2018, 03, 12},{12, 20, 00}}, "PM10", 170},
%%      {"Station 2", {{2018, 03, 12},{12, 40, 00}}, "PM10", 180},
%%      {"Station 2", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 200},
%%      {"Station 2", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 210},
%%
%%      {"Station 3", {{2018, 03, 12},{12, 20, 00}}, "PM10", 160},
%%      {"Station 3", {{2018, 03, 12},{12, 40, 00}}, "PM10", 150},
%%      {"Station 3", {{2018, 03, 12},{12, 20, 00}}, "PM2,5", 90},
%%      {"Station 3", {{2018, 03, 12},{12, 40, 00}}, "PM2,5", 110}
%%    ],
%%    P3
%%  ),
%%  P4.
