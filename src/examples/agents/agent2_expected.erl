-module(agent2_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_ia_snap/1,
         receive_ia_state/1,
         send_ai_close/1,
         send_ai_coord/1,
         send_ai_get/1,
         send_ai_set/1,
         start_link/0,
         state1/3,
         state2/3,
         state3/3,
         state5/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

%consume n
init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
%consume set
state1(internal, {send_ai_set, Ai_set}, Data) -> {next_state, state2, Data};
%consume get
state1(internal, {send_ai_get, Ai_get}, Data) -> {next_state, state5, Data};
%consume close
state1(internal, {send_ai_close, Ai_close}, Data) -> {stop, normal, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
%consume coord
state2(internal, {send_ai_coord, Ai_coord}, Data) -> {next_state, state3, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
state3(cast, {receive_ia_state, Ia_state}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

state5(enter, _OldState, _Data) -> keep_state_and_data;
%consume snap
state5(cast, {receive_ia_snap, Ia_snap}, Data) -> {stop, normal, Data}.

receive_ia_snap(Ia_snap) ->
    gen_statem:cast(?SERVER, {receive_ia_snap, Ia_snap}).

receive_ia_state(Ia_state) ->
    gen_statem:cast(?SERVER, {receive_ia_state, Ia_state}).

send_ai_close(Ai_close) ->
    gen_statem:internal(?SERVER, {send_ai_close, Ai_close}).

send_ai_coord(Ai_coord) ->
    gen_statem:internal(?SERVER, {send_ai_coord, Ai_coord}).

send_ai_get(Ai_get) -> gen_statem:internal(?SERVER, {send_ai_get, Ai_get}).

send_ai_set(Ai_set) -> gen_statem:internal(?SERVER, {send_ai_set, Ai_set}).

stop() -> gen_statem:stop(?SERVER).
