%%%-------------------------------------------------------------------
%%% @author jacob
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. May 2018 16:37
%%%-------------------------------------------------------------------
-module(pollution_server_otp_supervisor).
-author("jacob").
-behavior(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, pollutionServerSupervisor}, ?MODULE, []).

init(_InitVal) ->
  {ok, {
    {one_for_all, 2, 3},
    [
      {pollution_server_otp,
        {pollution_server_otp, start_link, []},
        permanent, brutal_kill, worker, [pollution_server_otp]}
    ]}
  }.
