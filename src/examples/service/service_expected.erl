-module(service_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_x/1,
         callback_mode/0,
         init/1,
         start_link/0,
         state1/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

%require n
init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_x, X}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_x(X) -> gen_statem:cast(?SERVER, {act_x, X}).

stop() -> gen_statem:stop(?SERVER).
