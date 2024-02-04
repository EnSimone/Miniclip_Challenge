%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(room_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting the room_supervisor ~n"),
  RoomChild = #{id => roomname,
    start => {room_gen_server, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [room_gen_server]},

  {ok, {#{strategy => one_for_one,
    intensity => 5,
    period => 30},
    [RoomChild]}
  }.
