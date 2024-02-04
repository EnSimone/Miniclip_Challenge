%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(user_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting the user_supervisor ~n"),
  UserChild = #{id => username,
    start => {user_gen_server, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [user_gen_server]},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [UserChild]}
  }.
