-module(auth_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_forbidden/1,
         act_ok/1,
         act_retry/1,
         callback_mode/0,
         init/1,
         receive_userpass/1,
         send_authenticate/1,
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
state1(internal, {send_authenticate, Authenticate}, Data) ->
    {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(cast, {receive_userpass, Userpass}, Data) -> {next_state, state3, Data}.

state3(enter, _OldState, _Data) -> keep_state_and_data;
%assert auth
state3(cast, {act_ok, Ok}, Data) -> {stop, normal, Data};
state3(cast, {act_retry, Retry}, Data) -> {next_state, state1, Data};
state3(cast, {act_forbidden, Forbidden}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_forbidden(Forbidden) ->
    gen_statem:cast(?SERVER, {act_forbidden, Forbidden}).

act_ok(Ok) -> gen_statem:cast(?SERVER, {act_ok, Ok}).

act_retry(Retry) -> gen_statem:cast(?SERVER, {act_retry, Retry}).

receive_userpass(Userpass) ->
    gen_statem:cast(?SERVER, {receive_userpass, Userpass}).

send_authenticate(Authenticate) ->
    gen_statem:internal(?SERVER, {send_authenticate, Authenticate}).

stop() -> gen_statem:stop(?SERVER).
