-module(s1_stub).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_s1/1,
         act_s2/1,
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
%assert one
state1(cast, {act_s1, S1}, Data) -> {stop, normal, Data};
%assert two
state1(cast, {act_s2, S2}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_s1(S1) -> gen_statem:cast(?SERVER, {act_s1, S1}).

act_s2(S2) -> gen_statem:cast(?SERVER, {act_s2, S2}).

stop() -> gen_statem:stop(?SERVER).