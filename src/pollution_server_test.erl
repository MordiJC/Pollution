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
  pollution_server:stop(),
  receive
    {response, {ok, _}} -> ok
  end.

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
  io:format("START!~n"),
  startServer(),
  pollution_server:addStation("Station 1", {0, 0}),
  handleResponse(),
  pollution_server:addStation("Station 2", {0, 0}),
  ?assert(handleResponse() =:= false),
  stopServer().
