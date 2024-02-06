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

-record(user_gen_server_state, {username, message, socket, next, name}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
%%-spec(start_link() ->
 %%{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

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
  random:seed({A,B,C}),
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
  send(AcceptSocket, "What's your username?", []),
  {noreply, S#user_gen_server_state{socket=AcceptSocket, next=name}};

handle_cast(room, S = #user_gen_server_state{socket=Socket}) ->
  send(Socket,
      "Room manager ~n"
      "Press 1 to create a room ~n"
      "Press 2 to list all available rooms ~n"
      "Press 3 to join an available room ~n"
      "Press 0 to delete a room (only if you are the creator) ~n",[]),
  {noreply, S#user_gen_server_state{next=room}};

handle_cast(create_room, S = #user_gen_server_state{socket=Socket}) ->
  send(Socket,
    "Which is the name of the room that you want to create? ~n",[]),
  {noreply, S#user_gen_server_state{next=create_room}};

handle_cast(delete_room, S = #user_gen_server_state{socket=Socket}) ->
  send(Socket,
    "Which is the name of the room that you want to delete? ~n",[]),
  {noreply, S#user_gen_server_state{next=delete_room}};

handle_cast(join_room, S = #user_gen_server_state{socket=Socket}) ->
  send(Socket,
    "Which is the name of the room that you want to join? ~n",[]),
  {noreply, S#user_gen_server_state{next=join_room}}.

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
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
  gen_server:cast(self(), room),
  {noreply, S#user_gen_server_state{name=Name, next=room}};

handle_info({tcp, _Socket, "quit"++_}, S) ->
  gen_tcp:close(S#user_gen_server_state.socket),
  {stop, normal, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=room}) ->
  case line(Str) of
    "1" ->
      gen_server:cast(self(), create_room);
    "2" ->
      gen_server:cast(self(), list_rooms);
    "3" ->
      gen_server:cast(self(), join_room);
    "0" ->
      gen_server:cast(self(), delete_room);
    _ -> % ask again because we didn't get what we wanted
      send(Socket, "Answer with 1 (create) or 2 (list) or 3 (join) or 0 (delete) ~n" , [])
  end,
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=create_room}) ->
  RoomName = line(Str),
  send(Socket,"You choose to create a room named ~s~n", [RoomName]),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=delete_room}) ->
  RoomName = line(Str),
  send(Socket,"You choose to delete a room named ~s~n", [RoomName]),
  {noreply, S};

handle_info({tcp, Socket, Str}, S = #user_gen_server_state{socket=Socket, next=join_room}) ->
  RoomName = line(Str),
  send(Socket,"You choose to join a room named ~s~n", [RoomName]),
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
