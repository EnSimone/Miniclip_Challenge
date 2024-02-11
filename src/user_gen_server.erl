%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. feb 2024 22:16
%%%-------------------------------------------------------------------
-module(user_gen_server).
-author("senri").


-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(user_gen_server_state, {username, message, socket, next, name, room_to_broadcast, user_for_message}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Args :: term()) ->
 {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
%%-spec(init(Args :: term()) ->
  %%{ok, State :: #user_gen_server_state{}} | {ok, State :: #user_gen_server_state{}, timeout() | hibernate} |
  %%{stop, Reason :: term()} | ignore).
init(Socket) ->
  io:format("Starting the user_gen_server ~n"),
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  random:seed({A,B,C}), %%using random:seed that is a deprecated because i did not find an equivalent function using rand
  gen_server:cast(self(), accept),
  {ok, #user_gen_server_state{socket=Socket}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #user_gen_server_state{}) ->
  {reply, Reply :: term(), NewState :: #user_gen_server_state{}} |
  {reply, Reply :: term(), NewState :: #user_gen_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #user_gen_server_state{}} |
  {noreply, NewState :: #user_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #user_gen_server_state{}} |
  {stop, Reason :: term(), NewState :: #user_gen_server_state{}}).
handle_call(_Request, _From, State = #user_gen_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #user_gen_server_state{}) ->
  {noreply, NewState :: #user_gen_server_state{}} |
  {noreply, NewState :: #user_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #user_gen_server_state{}}).
handle_cast(accept, S = #user_gen_server_state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  user_supervisor:start_socket(),
  Str = "What's your username? \r\n",
  send(AcceptSocket, Str, []),
  {noreply, S#user_gen_server_state{socket=AcceptSocket, next=name}};

handle_cast(duplicated_username, S = #user_gen_server_state{}) ->
  Str = "Insert a new username \r\n",
  send(S#user_gen_server_state.socket, Str, []),
  {noreply, S#user_gen_server_state{next=name}};

handle_cast(main_menu, S = #user_gen_server_state{socket=Socket}) ->
  Str =
    "Room manager:\r\n" ++
    "  Press 1 to create a public room\r\n" ++
    "  Press 2 to list all available rooms\r\n" ++
    "  Press 3 to join an available room\r\n" ++
    "  Press 4 to broadcast a message to all the users in the specified room\r\n" ++
    "  Press 5 to leave a room\r\n" ++
    "  Press 6 to send a message to a specific user\r\n" ++
    "  Press 7 to create a private room\r\n" ++
    "  Press 0 to delete a room (only if you are the creator)\r\n",
  send(Socket, Str, []),
  {noreply, S#user_gen_server_state{next=room_manager}};

handle_cast(create_room, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Which is the name of the room that you want to create? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=create_room}};

handle_cast(delete_room, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Which is the name of the room that you want to delete? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=delete_room}};

handle_cast(list_rooms, S = #user_gen_server_state{socket=Socket}) ->
  %%List all rooms
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_cast(join_room, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Which is the name of the room that you want to join? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=join_room}};

handle_cast(leave_room, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Which is the name of the room that you want to leave? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=leave_room}};

handle_cast({send_message, Message, Sender}, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Received message ~p from user ~p \r\n",
  send(Socket, Str,[Message, Sender]),
  {noreply, S};

handle_cast({send_message, Message, Sender, RoomName}, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Received message ~p from room ~p sent by ~p \r\n",
  send(Socket, Str,[Message, RoomName, Sender]),
  {noreply, S};

handle_cast(message_to_user, S = #user_gen_server_state{socket=Socket}) ->
  Str = "To which user do you want to send the message? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=user_for_message}};

handle_cast(message_request_for_user, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Write the message that you want to send \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=message_to_user_request}};

handle_cast(broadcast, S = #user_gen_server_state{socket=Socket}) ->
  Str = "In which room do you want to publish the message? \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=room_request}};

handle_cast(message_request, S = #user_gen_server_state{socket=Socket}) ->
  Str = "Write the message that you want to send \r\n",
  send(Socket, Str,[]),
  {noreply, S#user_gen_server_state{next=message_request}}.

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format("~n"++Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #user_gen_server_state{}) ->
  {noreply, NewState :: #user_gen_server_state{}} |
  {noreply, NewState :: #user_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #user_gen_server_state{}}).
handle_info({tcp, _Socket, Str}, S = #user_gen_server_state{next=name}) ->
  Name = line(Str),
  UserExists = username_exists(Name),
  if UserExists =:= false ->
    register(list_to_atom(Name),self()),
    gen_server:cast(self(), main_menu),
    {noreply, S#user_gen_server_state{name=Name, next=main_menu}};
  true ->
    Message = "Username ~s already exists, please select a new one\r\n",
    send(_Socket, Message , [Name]),
    gen_server:cast(self(), duplicated_username)
  end;

handle_info({tcp, _Socket, "quit"++_}, S) ->
  gen_tcp:close(S#user_gen_server_state.socket),
  {stop, normal, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=room_manager}) ->
  case line(Str) of
    "1" ->
      gen_server:cast(self(), create_room);
    "2" ->
      gen_server:cast(self(), list_rooms);
    "3" ->
      gen_server:cast(self(), join_room);
    "4" ->
      gen_server:cast(self(), broadcast);
    "5" ->
      gen_server:cast(self(), leave_room);
    "6" ->
      gen_server:cast(self(), message_to_user);
    "7" ->
      gen_server:cast(self(), create_private_room);
    "0" ->
      gen_server:cast(self(), delete_room);
    "quit"++_ ->
      gen_tcp:close(S#user_gen_server_state.socket),
      {stop, normal, S};
    _ -> % ask again because we didn't get what we wanted
      Message = "Answer with 1 (create) or 2 (list) or 3 (join) or 4 (broadcast) or 5 (leave) or 6 (message to user) or 7 (private room) or 0 (delete) \r\n",
      send(Socket, Message , [])
  end,
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=create_room}) ->
  RoomName = line(Str),
  Message = "You choose to create a room named ~s, creating and joining it.\r\n",
  send(Socket, Message, [RoomName]),
  room_supervisor:create_room(self(), RoomName, S#user_gen_server_state.name),
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=delete_room}) ->
  RoomName = line(Str),
  Message = "You choose to delete a room named ~s\r\n",
  send(Socket, Message, [RoomName]),
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=leave_room}) ->
  RoomName = line(Str),
  Message = "You choose to leave a room named ~s\r\n",
  send(Socket,Message, [RoomName]),
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=join_room}) ->
  RoomName = line(Str),
  Message = "You choose to join a room named ~s\r\n",
  send(Socket,Message, [RoomName]),
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{next=room_request}) ->
  RoomName = line(Str),
  gen_server:cast(self(), message_request),
  {noreply, S#user_gen_server_state{room_to_broadcast = RoomName}};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{next=user_for_message}) ->
  Username = line(Str),
  gen_server:cast(self(), message_request_for_user),
  {noreply, S#user_gen_server_state{user_for_message = Username}};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{room_to_broadcast = RoomName, next=message_request}) ->
  Message = line(Str),
  RoomPid = whereis(list_to_atom(RoomName)),
  case RoomPid of
    undefined -> io:format("The selected Room does not exist");
    _->gen_server:cast(RoomPid, {broadcast, S#user_gen_server_state.name, Message})
  end,
  gen_server:cast(self(), main_menu),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{user_for_message = Username, next=message_to_user_request}) ->
  Message = format(Str),
  FormattedMessage = replace(Message),
  UserPid = whereis(list_to_atom(Username)),
  case UserPid of
    undefined -> Log = ("The selected user does not exist, please insert an existing user \r\n"),
      send(Socket, Log, []),
      gen_server:cast(self(), message_to_user);
    _->gen_server:cast(UserPid, {send_message, FormattedMessage, S#user_gen_server_state.name}),
      gen_server:cast(self(),main_menu)
  end,
  {noreply, S};

handle_info({tcp_closed, _Socket}, S) ->
  {stop, normal, S};
handle_info({tcp_error, _Socket, _}, S) ->
  {stop, normal, S};
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

line(Str) ->
  hd(string:tokens(Str, "\r\n ")).

format(Str) ->
  hd(string:tokens(Str, "\n")).

replace(Str)->
  hd(string:replace(Str, "\r", "")).

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #user_gen_server_state{}) -> term()).
terminate(normal, _State = #user_gen_server_state{}) ->
  ok;
terminate(_Reason, _State = #user_gen_server_state{}) ->
  io:format("Terminate reason: ~p~n", [_Reason]).

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #user_gen_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #user_gen_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #user_gen_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
username_exists(Username) ->
  whereis(list_to_atom(Username)) =/= undefined.