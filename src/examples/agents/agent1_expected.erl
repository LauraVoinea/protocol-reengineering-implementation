-module(agent1_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_ua_close/1,
         receive_ua_coord/1,
         receive_ua_get/1,
         receive_ua_set_ua_set/1,
         send_au_snap/1,
         send_au_state/1,
         start_link/0,
         state1/3,
         state2/3,
         state3/3,
         state5/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
%assert n
%assert set
state1(cast, {receive_ua_set_ua_set, Ua_set_ua_set}, Data) ->
    {next_state, state2, Data};
%assert n
%assert get
state1(cast, {receive_ua_get, Ua_get}, Data) -> {next_state, state5, Data};
%assert n
%assert close
state1(cast, {receive_ua_close, Ua_close}, Data) -> {stop, normal, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
%assert coord
state2(cast, {receive_ua_coord, Ua_coord}, Data) -> {next_state, state3, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
state3(internal, {send_au_state, Au_state}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

state5(enter, _OldState, _Data) -> keep_state_and_data;
%assert snap
state5(internal, {send_au_snap, Au_snap}, Data) -> {stop, normal, Data}.

receive_ua_close(Ua_close) ->
    gen_statem:cast(?SERVER, {receive_ua_close, Ua_close}).

receive_ua_coord(Ua_coord) ->
    gen_statem:cast(?SERVER, {receive_ua_coord, Ua_coord}).

receive_ua_get(Ua_get) -> gen_statem:cast(?SERVER, {receive_ua_get, Ua_get}).

receive_ua_set_ua_set(Ua_set_ua_set) ->
    gen_statem:cast(?SERVER, {receive_ua_set_ua_set, Ua_set_ua_set}).

send_au_snap(Au_snap) -> gen_statem:internal(?SERVER, {send_au_snap, Au_snap}).

send_au_state(Au_state) ->
    gen_statem:internal(?SERVER, {send_au_state, Au_state}).

stop() -> gen_statem:stop(?SERVER).
