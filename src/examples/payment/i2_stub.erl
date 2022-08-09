-module(i2_stub).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_item/1,
         callback_mode/0,
         init/1,
         start_link/0,
         state1/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

%consume paid
init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_item, Item}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_item(Item) -> gen_statem:cast(?SERVER, {act_item, Item}).

stop() -> gen_statem:stop(?SERVER).