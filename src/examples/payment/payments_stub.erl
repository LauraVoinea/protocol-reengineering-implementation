-module(payments_stub).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_p1/1,
         act_p2/1,
         callback_mode/0,
         init/1,
         start_link/0,
         state1/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
%consume one
state1(cast, {act_p1, P1}, Data) -> {stop, normal, Data};
%consume two
state1(cast, {act_p2, P2}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_p1(P1) -> gen_statem:cast(?SERVER, {act_p1, P1}).

act_p2(P2) -> gen_statem:cast(?SERVER, {act_p2, P2}).

stop() -> gen_statem:stop(?SERVER).