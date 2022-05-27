-module(http_stub).

-behaviour(gen_statem).

-define(SERVER, ?MODULE).

-export([callback_mode/0,
         init/1,
         receive_acceptE/1,
         receive_acceptl/1,
         receive_acceptt/1,
         receive_body/1,
         receive_connection/1,
         receive_cookie/1,
         receive_dnt/1,
         receive_host/1,
         receive_request/1,
         receive_upgradeir/1,
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
         state12/3,
         state13/3,
         state14/3,
         state2/3,
         state27/3,
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
state2(cast, {receive_acceptE, AcceptE}, Data) -> {next_state, state2, Data};
state2(cast, {receive_dnt, Dnt}, Data) -> {next_state, state2, Data};
state2(cast, {receive_connection, Connection}, Data) ->
    {next_state, state2, Data};
state2(cast, {receive_upgradeir, Upgradeir}, Data) ->
    {next_state, state2, Data};
state2(cast, {receive_cookie, Cookie}, Data) -> {next_state, state2, Data};
%require auth
state2(cast, {receive_body, Body}, Data) -> {next_state, state12, Data}.

state12(enter, _OldState, _Data) -> keep_state_and_data;
state12(internal, {send_httpv, Httpv}, Data) -> {next_state, state13, Data}.

state13(enter, _OldState, _Data) -> keep_state_and_data;
state13(internal, {send_200, 200}, Data) -> {next_state, state14, Data};
state13(internal, {send_404, 404}, Data) -> {next_state, state27, Data}.

state14(enter, _OldState, _Data) -> keep_state_and_data;
state14(internal, {send_date, Date}, Data) -> {next_state, state14, Data};
state14(internal, {send_server, Server}, Data) -> {next_state, state14, Data};
state14(internal, {send_strictts, Strictts}, Data) ->
    {next_state, state14, Data};
state14(internal, {send_lastm, Lastm}, Data) -> {next_state, state14, Data};
state14(internal, {send_etag, Etag}, Data) -> {next_state, state14, Data};
state14(internal, {send_acceptr, Acceptr}, Data) -> {next_state, state14, Data};
state14(internal, {send_contentl, Contentl}, Data) ->
    {next_state, state14, Data};
state14(internal, {send_vary, Vary}, Data) -> {next_state, state14, Data};
state14(internal, {send_contentt, Contentt}, Data) ->
    {next_state, state14, Data};
state14(internal, {send_via, Via}, Data) -> {next_state, state14, Data};
state14(internal, {send_cache, Cache}, Data) -> {next_state, state14, Data};
state14(internal, {send_body, Body}, Data) -> {stop, normal, Data}.

terminate(_Reason, _State, _Data) -> ok.

state27(enter, _OldState, _Data) -> keep_state_and_data;
state27(internal, {send_date, Date}, Data) -> {next_state, state27, Data};
state27(internal, {send_server, Server}, Data) -> {next_state, state27, Data};
state27(internal, {send_strictts, Strictts}, Data) ->
    {next_state, state27, Data};
state27(internal, {send_lastm, Lastm}, Data) -> {next_state, state27, Data};
state27(internal, {send_etag, Etag}, Data) -> {next_state, state27, Data};
state27(internal, {send_acceptr, Acceptr}, Data) -> {next_state, state27, Data};
state27(internal, {send_contentl, Contentl}, Data) ->
    {next_state, state27, Data};
state27(internal, {send_vary, Vary}, Data) -> {next_state, state27, Data};
state27(internal, {send_contentt, Contentt}, Data) ->
    {next_state, state27, Data};
state27(internal, {send_via, Via}, Data) -> {next_state, state27, Data};
state27(internal, {send_cache, Cache}, Data) -> {next_state, state27, Data};
state27(internal, {send_body, Body}, Data) -> {stop, normal, Data}.

receive_acceptE(AcceptE) ->
    gen_statem:cast(?SERVER, {receive_acceptE, AcceptE}).

receive_acceptl(Acceptl) ->
    gen_statem:cast(?SERVER, {receive_acceptl, Acceptl}).

receive_acceptt(Acceptt) ->
    gen_statem:cast(?SERVER, {receive_acceptt, Acceptt}).

receive_body(Body) -> gen_statem:cast(?SERVER, {receive_body, Body}).

receive_connection(Connection) ->
    gen_statem:cast(?SERVER, {receive_connection, Connection}).

receive_cookie(Cookie) -> gen_statem:cast(?SERVER, {receive_cookie, Cookie}).

receive_dnt(Dnt) -> gen_statem:cast(?SERVER, {receive_dnt, Dnt}).

receive_host(Host) -> gen_statem:cast(?SERVER, {receive_host, Host}).

receive_request(Request) ->
    gen_statem:cast(?SERVER, {receive_request, Request}).

receive_upgradeir(Upgradeir) ->
    gen_statem:cast(?SERVER, {receive_upgradeir, Upgradeir}).

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