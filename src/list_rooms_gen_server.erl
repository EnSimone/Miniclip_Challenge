%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. feb 2024 23:09
%%%-------------------------------------------------------------------
-module(list_rooms_gen_server).
-author("senri").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, add/1, remove/1, list/1]).

-define(SERVER, ?MODULE).

-record(list_rooms_gen_server_state, {rooms}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #list_rooms_gen_server_state{}} | {ok, State :: #list_rooms_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Starting list_room_gen_server~n"),
  register(rooms,self()),
  {ok, #list_rooms_gen_server_state{rooms = []}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #list_rooms_gen_server_state{}) ->
  {reply, Reply :: term(), NewState :: #list_rooms_gen_server_state{}} |
  {reply, Reply :: term(), NewState :: #list_rooms_gen_server_state{}, timeout() | hibernate} |
  {noreply, NewState :: #list_rooms_gen_server_state{}} |
  {noreply, NewState :: #list_rooms_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #list_rooms_gen_server_state{}} |
  {stop, Reason :: term(), NewState :: #list_rooms_gen_server_state{}}).
handle_call(_Request, _From, State = #list_rooms_gen_server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #list_rooms_gen_server_state{}) ->
  {noreply, NewState :: #list_rooms_gen_server_state{}} |
  {noreply, NewState :: #list_rooms_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #list_rooms_gen_server_state{}}).
handle_cast({add, RoomName}, State = #list_rooms_gen_server_state{rooms=RoomList}) ->
    {noreply, State#list_rooms_gen_server_state{rooms = [RoomName | RoomList]}};

handle_cast({remove, RoomName}, State = #list_rooms_gen_server_state{rooms=RoomList}) ->
    {noreply, State#list_rooms_gen_server_state{rooms = RoomList--[RoomName]}};

handle_cast({list, UserPid}, State = #list_rooms_gen_server_state{rooms=RoomList}) ->
  Message = "The available rooms are:\r\n ~p\r\n",
  gen_server:cast(UserPid,{send_message, Message, RoomList}),
  {noreply, State};

handle_cast(_Request, State = #list_rooms_gen_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #list_rooms_gen_server_state{}) ->
  {noreply, NewState :: #list_rooms_gen_server_state{}} |
  {noreply, NewState :: #list_rooms_gen_server_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #list_rooms_gen_server_state{}}).
handle_info(_Info, State = #list_rooms_gen_server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #list_rooms_gen_server_state{}) -> term()).
terminate(_Reason, _State = #list_rooms_gen_server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #list_rooms_gen_server_state{},
    Extra :: term()) ->
  {ok, NewState :: #list_rooms_gen_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #list_rooms_gen_server_state{}, _Extra) ->
  {ok, State}.

add(RoomName)->
  gen_server:cast(whereis(rooms),{add, RoomName}),
  ok.

remove(RoomName)->
  gen_server:cast(whereis(rooms),{remove, RoomName}),
  ok.

list(UserPid)->
  gen_server:cast(whereis(rooms),{list, UserPid}),
  ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
