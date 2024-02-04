%%%-------------------------------------------------------------------
%% @doc chat_server_challenge public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_challenge_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chat_server_challenge_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
