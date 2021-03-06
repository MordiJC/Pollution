%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2018 13:14
%%%-------------------------------------------------------------------
-module(pollution_server_supervisor).
-author("jacob").

%% API
-export([start/0, init/0]).

start() ->
  spawn(?MODULE, init, []).

init() ->
  process_flag(trap_exit, true),
  pollution_server:start_link(),
  loop().

loop() ->
  receive
    {'EXIT', _, _} ->
      pollution_server:start_link(),
      loop()
  end.
