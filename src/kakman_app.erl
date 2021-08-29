%%%-------------------------------------------------------------------
%% @doc kakman public API
%% @end
%%%-------------------------------------------------------------------

-module(kakman_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kakman_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
