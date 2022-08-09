-module(tan_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_fail/1,
         act_ok/1,
         callback_mode/0,
         init/1,
         receive_tan/1,
         send_id/1,
         start_link/0,
         state1/3,
         state2/3,
         state3/3,
         stop/0]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

%require pin
init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(internal, {send_id, Id}, Data) -> {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(cast, {receive_tan, Tan}, Data) -> {next_state, state3, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
%assert tan
state3(cast, {act_ok, Ok}, Data) -> {next_state, state1, Data};
state3(cast, {act_fail, Fail}, Data) -> {next_state, state1, Data}.

act_fail(Fail) -> gen_statem:cast(?SERVER, {act_fail, Fail}).

act_ok(Ok) -> gen_statem:cast(?SERVER, {act_ok, Ok}).

receive_tan(Tan) -> gen_statem:cast(?SERVER, {receive_tan, Tan}).

send_id(Id) -> gen_statem:internal(?SERVER, {send_id, Id}).

stop() -> gen_statem:stop(?SERVER).
