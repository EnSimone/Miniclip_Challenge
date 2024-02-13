%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. feb 2024 22:17
%%%-------------------------------------------------------------------
-module(room_gen_server).
-author("senri").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, join_room/3, leave_room/3, delete_room/3, invite_room/4]).

-define(SERVER, ?MODULE).

-record(room_gen_server_state, {name, creator, connected_users, ispublic}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(_Arg0::term(), _Arg1::term(), _Arg2::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(RoomName, RoomCreatorUsername, IsPublic) ->
  gen_server:start_link(?MODULE, [RoomName, RoomCreatorUsername, IsPublic] , []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================a

%% @private
%% @doc Initializes the server
-spec(init(_Args::term()) ->
  {ok, State :: #room_gen_server_state{}} | {ok, State :: #room_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([RoomName, RoomCreatorUsername, IsPublic]) ->
  io:format("Starting room_gen_server ~s~n",[RoomName]),
  register(list_to_atom(RoomName),self()),
  {ok, #room_gen_server_state{name = RoomName,creator = RoomCreatorUsername, connected_users = [RoomCreatorUsername], ispublic = IsPublic}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #room_gen_server_state{}) ->
  {reply, Reply :: term(), NewState :: #room_gen_server_state{}} |
  {reply, Reply :: term(), NewState :: #room_gen_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #room_gen_server_state{}} |
  {noreply, NewState :: #room_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #room_gen_server_state{}} |
  {stop, Reason :: term(), NewState :: #room_gen_server_state{}}).
handle_call(_Request, _From, State = #room_gen_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #room_gen_server_state{}) ->
  {noreply, NewState :: #room_gen_server_state{}} |
  {noreply, NewState :: #room_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #room_gen_server_state{}}).

handle_cast({broadcast, Sender, Message}, State = #room_gen_server_state{connected_users=Users}) ->
  UserPid = whereis(list_to_atom(Sender)),
  Member = lists:member(Sender, Users),
  if Member =:= false ->
    Str = "You can not publish a message in a room, you did not join it, please join the room before sending it\r\n",
    gen_server:cast(UserPid, {send_message, Str}),
    {noreply, State};
    true ->
      lists:foreach(
        fun(Username)->
          UserPidDestination = whereis(list_to_atom(Username)),
          gen_server:cast(UserPidDestination, {send_message, Message, Sender, State#room_gen_server_state.name})
          end,
        Users
      ),
      {noreply, State}
  end;

handle_cast({join, UserPid, Username}, State = #room_gen_server_state{connected_users=Users, ispublic = IsPublic}) ->
  if IsPublic =:= true ->
    Member = lists:member(Username, Users),
    if Member =:= false ->
      Str = "Hello, i joined the room",
      gen_server:cast(self(), {broadcast, Username,Str}),
      {noreply, State#room_gen_server_state{connected_users = [Username | Users]}};
      true ->
        Message = "You already joined the room ~s\r\n",
        gen_server:cast(UserPid, {send_message, Message, State#room_gen_server_state.name}),
        {noreply, State}
    end;
    true ->
      ErrorMessage = "The room ~s is private, you can not join without an invite\r\n",
      gen_server:cast(UserPid, {send_message, ErrorMessage, State#room_gen_server_state.name}),
      {noreply, State}
  end;

handle_cast({invite, UserPid, Username, InvitedUsername}, State = #room_gen_server_state{connected_users=Users}) ->
  Member = lists:member(Username, Users),
  if Member =:= true ->
    InvitedMember = lists:member(InvitedUsername, Users),
    if InvitedMember  =:= false ->
      MessageToInvitedUser = "You have been invited to join the room ~s, joining it...\r\n",
      InviteUserPid = whereis(list_to_atom(InvitedUsername)),
      gen_server:cast(InviteUserPid, {send_message, MessageToInvitedUser, State#room_gen_server_state.name}),
      Str = "Hello, i joined the room",
      gen_server:cast(self(), {broadcast, InvitedUsername,Str}),
      {noreply, State#room_gen_server_state{connected_users = [InvitedUsername | Users]}};
    true ->
      Message = "The user ~s already joined the room ~s\r\n",
      gen_server:cast(UserPid, {send_message, Message, [InvitedUsername, State#room_gen_server_state.name]}),
      {noreply, State}
    end;
    true ->
      MessageToInviter = "You are not a member of the room ~s, you can not invite other users\r\n",
      gen_server:cast(UserPid, {send_message, MessageToInviter, State#room_gen_server_state.name})
  end;

handle_cast({leave, UserPid, Username}, State = #room_gen_server_state{connected_users=Users}) ->
  Member = lists:member(Username, Users),
  if Member =:= false ->
    Message = "You were not a member of the room ~s\r\n",
    gen_server:cast(UserPid, {send_message, Message, State#room_gen_server_state.name}),
    {noreply, State};
    true ->
    NewUsers = Users--[Username],
    Str = "Goodbye, you left the room ~s\r\n",
    gen_server:cast(UserPid, {send_message, Str, State#room_gen_server_state.name}),
    {noreply, State#room_gen_server_state{connected_users = NewUsers}}
  end;

handle_cast({delete, UserPid, Username}, State = #room_gen_server_state{creator = Creator}) ->
  if Username =:= Creator ->
    Str = "You deleted the room ~s\r\n",
    gen_server:cast(UserPid, {send_message, Str, State#room_gen_server_state.name}),
    {stop, normal, State};
    true ->
      Message = "You are not the creator of the room ~s\r\n",
      gen_server:cast(UserPid, {send_message, Message, State#room_gen_server_state.name}),
      {noreply, State}
  end;

handle_cast(_Request, State = #room_gen_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #room_gen_server_state{}) ->
  {noreply, NewState :: #room_gen_server_state{}} |
  {noreply, NewState :: #room_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #room_gen_server_state{}}).
handle_info(_Info, State = #room_gen_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #room_gen_server_state{}) -> term()).
terminate(_Reason, _State = #room_gen_server_state{}) ->
  if _State#room_gen_server_state.ispublic =:= true ->
    list_rooms_gen_server:remove(_State#room_gen_server_state.name)
  end,
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #room_gen_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #room_gen_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #room_gen_server_state{}, _Extra) ->
  {ok, State}.

join_room(UserPid, RoomName, Username) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom(RoomName)), {join, UserPid, Username});
    true ->
      Str = "Room ~s does not exist, select another room\r\n",
      gen_server:cast(UserPid, {send_message, Str, RoomName})
  end.

leave_room(UserPid, RoomName, Username) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom(RoomName)), {leave, UserPid, Username});
    true ->
      Str = "Room ~s does not exist, select another room\r\n",
      gen_server:cast(UserPid, {send_message, Str, RoomName})
  end.

delete_room(UserPid, RoomName, Username) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom(RoomName)), {delete, UserPid, Username});
    true ->
      Str = "Room ~s does not exist, select another room\r\n",
      gen_server:cast(UserPid, {send_message, Str, RoomName})
  end.

invite_room(Username, UserPid, RoomName, InvitedUsername) ->
  RoomExists = room_exists(RoomName),
  if RoomExists =:= true ->
    gen_server:cast(whereis(list_to_atom(RoomName)), {invite, UserPid, Username, InvitedUsername});
    true ->
      Str = "Room ~s does not exist, select another room\r\n",
      gen_server:cast(UserPid, {send_message, Str, RoomName})
  end.

room_exists(RoomName) ->
  whereis(list_to_atom(RoomName)) =/= undefined.
%%%===================================================================
%%% Internal functions
%%%===================================================================

