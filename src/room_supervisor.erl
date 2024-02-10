%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(room_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([create_room/3, list_rooms/1, delete_room/2]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting the room_supervisor ~n"),
  SupFlags = #{strategy => simple_one_for_one,
    intensity => 1000,
    period => 3600},

  RoomChild = #{id => 'room',
    start => {room_gen_server, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [room_gen_server]},

  {ok, {SupFlags, [RoomChild]}}.

room_exists(RoomName) ->
  whereis(list_to_atom(RoomName)) =/= undefined.

create_room(UserPid, RoomName, RoomCreatorUsername) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= false ->
    io:format("Room named ~s does not exist, creating it ~n", [RoomName]),
    _AChild = #{id => 'room',
      start => {'room_gen_server', start_link, [RoomName, RoomCreatorUsername]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => ['room_gen_server']},
    case supervisor:start_child(room_gen_server, [RoomName, RoomCreatorUsername]) of
      {ok, RoomPid} ->
        register(list_to_atom(RoomName),RoomPid),
        Str = io:format("Room ~s created ~n", [RoomName]),
        gen_server:cast(UserPid, {send_message, Str});
      _ -> io:format("Error starting the room_gen_server")
    end;
    true ->
      Str = io:format("Room with name ~s already exists - consider joining it!", [RoomName]),
      gen_server:cast(UserPid, {send_message, Str})
  end.

list_rooms(_Arg0) ->
  %%erlang:error(not_implemented)
  ok.

delete_room(_Arg0, _Arg1) ->
  %%erlang:error(not_implemented)
  ok.