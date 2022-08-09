-module(reeng).

-behaviour(gen_server).

%% API
-export([start_link/0, compose/3, factorize/2, generate/2, extract/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

compose(FirstProtocol, SecondProtocol, WeakFlag)->
  gen_server:call(?MODULE, {compose, FirstProtocol, SecondProtocol, WeakFlag}).

factorize(FirstProtocol, SecondProtocol)->
  gen_server:call(?MODULE, {factorize, FirstProtocol, SecondProtocol}).

generate(Protocol, FileName)->
  gen_server:call(?MODULE, {generate, Protocol, FileName}).

extract(FileName)->
  gen_server:call(?MODULE, {extract, FileName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call({compose, FirstProtocol, SecondProtocol, strong}, _From, State) ->
  Reply = interleave:interleave(FirstProtocol, SecondProtocol),
  {reply, Reply, State};

handle_call({compose, FirstProtocol, SecondProtocol, weak}, _From, State) ->
  Reply = lists:usort(interleave:interleaveWeak(FirstProtocol, SecondProtocol)),
  {reply, Reply, State};

handle_call({compose, FirstProtocol, SecondProtocol, correlating}, _From, State) ->
  Reply = lists:usort(interleave:interleaveCorrelating(FirstProtocol, SecondProtocol)),
  {reply, Reply, State};

handle_call({compose, FirstProtocol, SecondProtocol, all}, _From, State) ->
    Reply = lists:usort(interleave:interleaveAll(FirstProtocol, SecondProtocol)),
    {reply, Reply, State};

handle_call({factorize, FirstProtocol, SecondProtocol}, _From, State) ->
  Reply = factorize:fact(FirstProtocol, SecondProtocol),
  {reply, Reply, State};

handle_call({generate, Protocol, FileName}, _From, State) ->
  Reply = generate:gen(Protocol, FileName),
  {reply, Reply, State};

handle_call({extract, FileName}, _From, State) ->
  Reply = extract:protocol(FileName),
  {reply, Reply, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
