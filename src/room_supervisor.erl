%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(room_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([create_room/4]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting room_supervisor ~n"),
  SupFlags = #{strategy => simple_one_for_one,
    intensity => 1000,
    period => 3600},

  RoomChild = #{id => 'room',
    start => {room_gen_server, start_link, []},
    restart => transient,
    shutdown => 2000,
    type => worker,
    modules => [room_gen_server]},

  {ok, {SupFlags, [RoomChild]}}.

room_exists(RoomName) ->
  whereis(list_to_atom(RoomName)) =/= undefined.

create_room(UserPid, RoomName, RoomCreatorUsername, IsPublic) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= false ->
    io:format("Room named ~s does not exist, creating it ~n", [RoomName]),
    case supervisor:start_child(?MODULE, [RoomName, RoomCreatorUsername, IsPublic]) of
      {ok, _} ->
        Str = ("Room ~s created\r\n"),
        case IsPublic of
          true -> list_rooms_gen_server:add(RoomName);
          _ -> nothing
        end,
        gen_server:cast(UserPid, {send_message, Str, RoomName});
      _ -> io:format("Error starting the room_gen_server")
    end;
    true ->
      Str = "Room with name ~s already exists, please select a new room's name or join the selected one\r\n",
      gen_server:cast(UserPid, {send_message, Str, RoomName})
  end.

