-module(bank_stub).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([act_logout/1,
         act_payment/1,
         act_statement/1,
         callback_mode/0,
         init/1,
         receive_details/1,
         send_statement/1,
         start_link/0,
         state1/3,
         state2/3,
         state4/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

%require pin
init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {act_statement, Statement}, Data) -> {next_state, state2, Data};
%assert pay
%consume tan
state1(cast, {act_payment, Payment}, Data) -> {next_state, state4, Data};
%consume pin
state1(cast, {act_logout, Logout}, Data) -> {stop, normal, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(internal, {send_statement, Statement}, Data) ->
    {next_state, state1, Data}.

state4(enter, _OldState, _Data) -> keep_state_and_data;
state4(cast, {receive_details, Details}, Data) -> {next_state, state1, Data}.

terminate(_Reason, _State, _Data) -> ok.

act_logout(Logout) -> gen_statem:cast(?SERVER, {act_logout, Logout}).

act_payment(Payment) -> gen_statem:cast(?SERVER, {act_payment, Payment}).

act_statement(Statement) ->
    gen_statem:cast(?SERVER, {act_statement, Statement}).

receive_details(Details) ->
    gen_statem:cast(?SERVER, {receive_details, Details}).

send_statement(Statement) ->
    gen_statem:internal(?SERVER, {send_statement, Statement}).

stop() -> gen_statem:stop(?SERVER).