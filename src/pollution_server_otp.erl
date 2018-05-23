%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 13:53
%%%-------------------------------------------------------------------
-module(pollution_server_otp).
-author("jacob").

-behaviour(gen_server).
%% API
-export([start_link/0, start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getDeviation/2, getMonitor/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% START %%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, 0, []).

stop() ->
  gen_server:call(?MODULE, terminate).


%% CLIENT -> SERVER interface

addStation(Name, Position) -> gen_server:call(?MODULE, {addStation, Name, Position}).

addValue(PositionOrName, Time, Type, Value) ->
  gen_server:call(?MODULE, {addValue, PositionOrName, Time, Type, Value}).

removeValue(PositionOrName, Time, Type) ->
  gen_server:call(?MODULE, {removeValue, PositionOrName, Time, Type}).

getOneValue(PositionOrName, Time, Type) ->
  gen_server:call(?MODULE, {getOneValue, PositionOrName, Time, Type}).

getStationMean(PositionOrName, Type) ->
  gen_server:call(?MODULE, {getStationMean, PositionOrName, Type}).

getDailyMean(Type, Date) ->
  gen_server:call(?MODULE, {getDailyMean, Type, Date}).

getDeviation(Type, Hour) ->
  gen_server:call(?MODULE, {getDeviation, Type, Hour}).

getMonitor() ->
  gen_server:call(?MODULE, {getMonitor}).

%% gen_server

init(_Args) -> {ok, pollution:createMonitor()}.

handle_call({addStation, Name, Position}, _From, Monitor) ->
  case pollution:addStation(Name, Position, Monitor) of
    {response, }
  end.

handle_cast(inc, N) -> {noreply, N + 1}.

handle_call(get, _From, N) -> {reply, N, N};
handle_call(terminate, _From, N) -> {stop, normal, ok, N}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n", [N]), ok.