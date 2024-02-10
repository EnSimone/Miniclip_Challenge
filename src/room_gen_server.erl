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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(room_gen_server_state, {name, creator, connected_users}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(_Args::term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([RoomName, RoomCreatorUsername]) ->
  gen_server:start_link(?MODULE, [RoomName, RoomCreatorUsername],[]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================a

%% @private
%% @doc Initializes the server
-spec(init(_Args::term()) ->
  {ok, State :: #room_gen_server_state{}} | {ok, State :: #room_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([RoomName, RoomCreatorUsername]) ->
  io:format("Starting the room_gen_server ~n"),
  {ok, #room_gen_server_state{name = RoomName,creator = RoomCreatorUsername, connected_users = [RoomCreatorUsername]}}.

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
  lists:foreach(
    fun(Username)->
      UserPid = whereis(list_to_atom(Username)),
      gen_server:cast(UserPid, {send_message, Message, Sender, State#room_gen_server_state.name})
      end,
    Users
  ),
  {noreply, State};
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
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #room_gen_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #room_gen_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #room_gen_server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

