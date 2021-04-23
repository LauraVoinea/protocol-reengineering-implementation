%%%-------------------------------------------------------------------
%% @doc protocol-reengineering-implementation public API
%% @end
%%%-------------------------------------------------------------------

-module(protocol_reengineering_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    protocol-reengineering-implementation_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
