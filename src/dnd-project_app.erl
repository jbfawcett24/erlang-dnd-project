%%%-------------------------------------------------------------------
%% @doc dnd-project public API
%% @end
%%%-------------------------------------------------------------------

-module(dnd-project_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dnd-project_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
