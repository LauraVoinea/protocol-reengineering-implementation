-module(resource_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_l/1,
         act_m/1,
         act_r/1,
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
%assert n
state1(cast, {act_l, L}, Data) -> {stop, normal, Data};
%assert n
state1(cast, {act_r, R}, Data) -> {stop, normal, Data};
%assert n
state1(cast, {act_m, M}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_l(L) -> gen_statem:cast(?SERVER, {act_l, L}).

act_m(M) -> gen_statem:cast(?SERVER, {act_m, M}).

act_r(R) -> gen_statem:cast(?SERVER, {act_r, R}).

stop() -> gen_statem:stop(?SERVER).
