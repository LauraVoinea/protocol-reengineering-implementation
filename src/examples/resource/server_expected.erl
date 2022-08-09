-module(server_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_accept/1,
         act_b/1,
         act_ignore/1,
         act_request/1,
         callback_mode/0,
         init/1,
         start_link/0,
         state1/3,
         state2/3,
         state3/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_request, Request}, Data) -> {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(cast, {act_accept, Accept}, Data) -> {next_state, state3, Data};
state2(cast, {act_ignore, Ignore}, Data) -> {next_state, state1, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
%require n
state3(cast, {act_b, B}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_accept(Accept) -> gen_statem:cast(?SERVER, {act_accept, Accept}).

act_b(B) -> gen_statem:cast(?SERVER, {act_b, B}).

act_ignore(Ignore) -> gen_statem:cast(?SERVER, {act_ignore, Ignore}).

act_request(Request) -> gen_statem:cast(?SERVER, {act_request, Request}).

stop() -> gen_statem:stop(?SERVER).
