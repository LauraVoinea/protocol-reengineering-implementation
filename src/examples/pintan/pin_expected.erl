-module(pin_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_fail/1,
         act_ok/1,
         callback_mode/0,
         init/1,
         receive_pin/1,
         start_link/0,
         state1/3,
         state2/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {receive_pin, Pin}, Data) -> {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
%assert pin
state2(cast, {act_ok, Ok}, Data) -> {stop, normal, Data};
state2(cast, {act_fail, Fail}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_fail(Fail) -> gen_statem:cast(?SERVER, {act_fail, Fail}).

act_ok(Ok) -> gen_statem:cast(?SERVER, {act_ok, Ok}).

receive_pin(Pin) -> gen_statem:cast(?SERVER, {receive_pin, Pin}).

stop() -> gen_statem:stop(?SERVER).
