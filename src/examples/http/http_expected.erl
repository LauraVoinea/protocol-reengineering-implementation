-module(http_expected).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_acceptl/1,
         receive_acceptt/1,
         receive_body/1,
         receive_host/1,
         receive_request/1,
         receive_usera/1,
         send_200/1,
         send_404/1,
         send_acceptr/1,
         send_body/1,
         send_cache/1,
         send_contentl/1,
         send_contentt/1,
         send_date/1,
         send_etag/1,
         send_httpv/1,
         send_lastm/1,
         send_server/1,
         send_strictts/1,
         send_vary/1,
         send_via/1,
         start_link/0,
         state1/3,
         state2/3,
         state22/3,
         state7/3,
         state8/3,
         state9/3,
         stop/0,
         terminate/3]).

start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions, state_enter].

init([]) -> {ok, state1, {}}.

state1(enter, _OldState, _Data) -> keep_state_and_data;
state1(cast, {receive_request, Request}, Data) -> {next_state, state2, Data}.

state2(enter, _OldState, _Data) -> keep_state_and_data;
state2(cast, {receive_host, Host}, Data) -> {next_state, state2, Data};
state2(cast, {receive_usera, Usera}, Data) -> {next_state, state2, Data};
state2(cast, {receive_acceptt, Acceptt}, Data) -> {next_state, state2, Data};
state2(cast, {receive_acceptl, Acceptl}, Data) -> {next_state, state2, Data};
%require auth
state2(cast, {receive_body, Body}, Data) -> {next_state, state7, Data}.

state7(enter, _OldState, _Data) -> keep_state_and_data;
state7(internal, {send_httpv, Httpv}, Data) -> {next_state, state8, Data}.

state8(enter, _OldState, _Data) -> keep_state_and_data;
state8(internal, {send_200, 200}, Data) -> {next_state, state9, Data};
state8(internal, {send_404, 404}, Data) -> {next_state, state22, Data}.

state9(enter, _OldState, _Data) -> keep_state_and_data;
state9(internal, {send_date, Date}, Data) -> {next_state, state9, Data};
state9(internal, {send_server, Server}, Data) -> {next_state, state9, Data};
state9(internal, {send_strictts, Strictts}, Data) -> {next_state, state9, Data};
state9(internal, {send_lastm, Lastm}, Data) -> {next_state, state9, Data};
state9(internal, {send_etag, Etag}, Data) -> {next_state, state9, Data};
state9(internal, {send_acceptr, Acceptr}, Data) -> {next_state, state9, Data};
state9(internal, {send_contentl, Contentl}, Data) -> {next_state, state9, Data};
state9(internal, {send_vary, Vary}, Data) -> {next_state, state9, Data};
state9(internal, {send_contentt, Contentt}, Data) -> {next_state, state9, Data};
state9(internal, {send_via, Via}, Data) -> {next_state, state9, Data};
state9(internal, {send_cache, Cache}, Data) -> {next_state, state9, Data};
state9(internal, {send_body, Body}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

state22(enter, _OldState, _Data) -> keep_state_and_data;
state22(internal, {send_date, Date}, Data) -> {next_state, state22, Data};
state22(internal, {send_server, Server}, Data) -> {next_state, state22, Data};
state22(internal, {send_strictts, Strictts}, Data) ->
    {next_state, state22, Data};
state22(internal, {send_lastm, Lastm}, Data) -> {next_state, state22, Data};
state22(internal, {send_etag, Etag}, Data) -> {next_state, state22, Data};
state22(internal, {send_acceptr, Acceptr}, Data) -> {next_state, state22, Data};
state22(internal, {send_contentl, Contentl}, Data) ->
    {next_state, state22, Data};
state22(internal, {send_vary, Vary}, Data) -> {next_state, state22, Data};
state22(internal, {send_contentt, Contentt}, Data) ->
    {next_state, state22, Data};
state22(internal, {send_via, Via}, Data) -> {next_state, state22, Data};
state22(internal, {send_cache, Cache}, Data) -> {next_state, state22, Data};
state22(internal, {send_body, Body}, Data) -> {stop, normal, Data}.

receive_acceptl(Acceptl) ->
    gen_statem:cast(?SERVER, {receive_acceptl, Acceptl}).

receive_acceptt(Acceptt) ->
    gen_statem:cast(?SERVER, {receive_acceptt, Acceptt}).

receive_body(Body) -> gen_statem:cast(?SERVER, {receive_body, Body}).

receive_host(Host) -> gen_statem:cast(?SERVER, {receive_host, Host}).

receive_request(Request) ->
    gen_statem:cast(?SERVER, {receive_request, Request}).

receive_usera(Usera) -> gen_statem:cast(?SERVER, {receive_usera, Usera}).

send_200(200) -> gen_statem:internal(?SERVER, {send_200, 200}).

send_404(404) -> gen_statem:internal(?SERVER, {send_404, 404}).

send_acceptr(Acceptr) -> gen_statem:internal(?SERVER, {send_acceptr, Acceptr}).

send_body(Body) -> gen_statem:internal(?SERVER, {send_body, Body}).

send_cache(Cache) -> gen_statem:internal(?SERVER, {send_cache, Cache}).

send_contentl(Contentl) ->
    gen_statem:internal(?SERVER, {send_contentl, Contentl}).

send_contentt(Contentt) ->
    gen_statem:internal(?SERVER, {send_contentt, Contentt}).

send_date(Date) -> gen_statem:internal(?SERVER, {send_date, Date}).

send_etag(Etag) -> gen_statem:internal(?SERVER, {send_etag, Etag}).

send_httpv(Httpv) -> gen_statem:internal(?SERVER, {send_httpv, Httpv}).

send_lastm(Lastm) -> gen_statem:internal(?SERVER, {send_lastm, Lastm}).

send_server(Server) -> gen_statem:internal(?SERVER, {send_server, Server}).

send_strictts(Strictts) ->
    gen_statem:internal(?SERVER, {send_strictts, Strictts}).

send_vary(Vary) -> gen_statem:internal(?SERVER, {send_vary, Vary}).

send_via(Via) -> gen_statem:internal(?SERVER, {send_via, Via}).

stop() -> gen_statem:stop(?SERVER).
