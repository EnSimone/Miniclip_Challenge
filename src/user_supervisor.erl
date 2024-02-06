%%%-------------------------------------------------------------------
%%% @author senri
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(user_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1, start_socket/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("Starting the user_supervisor ~n"),
  {ok, ListenSocket} = gen_tcp:listen(8080, [{active,once}, {packet,line}]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
    [{socket,
      {user_gen_server, start_link, [ListenSocket]}, % pass the socket!
      temporary, 1000, worker, [user_gen_server]}
    ]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

%% Start with 20 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.
