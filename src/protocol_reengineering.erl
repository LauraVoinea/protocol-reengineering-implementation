-module(protocol_reengineering).

-behaviour(gen_server).

%% API
-export([start_link/0, compose/3, factorize/2, generate/3, extract/2]).

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
  gen_server:cast(?MODULE, {compose, FirstProtocol, SecondProtocol, WeakFlag}).

factorize(FirstProtocol, SecondProtocol)->
  gen_server:cast(?MODULE, {factorize, FirstProtocol, SecondProtocol}).

generate(Protocol, FileName, Path)->
  gen_server:cast(?MODULE, {generate, Protocol, FileName, Path}).

extract(FileName, Path)->
  gen_server:cast(?MODULE, {extract, FileName, Path}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({compose, FirstProtocol, SecondProtocol, strong}, State) ->
  interleave:interleave(FirstProtocol, SecondProtocol),
  {noreply, State};

handle_cast({compose, FirstProtocol, SecondProtocol, weak}, State) ->
  interleave:interleaveWeak(FirstProtocol, SecondProtocol),
  {noreply, State};

handle_cast({factorize, FirstProtocol, SecondProtocol}, State) ->
  factorize:fact(FirstProtocol, SecondProtocol),
  {noreply, State};

handle_cast({generate, Protocol, FileName, Path}, State) ->
  Forms = generate:gen_module(FileName, Protocol),
  file:write_file(lists:concat([FileName, ".erl"]),
                  erl_prettypr:format(erl_syntax:form_list(Forms),
                                      [{paper,160},{ribbon,80}])),
  {noreply, State};

handle_cast({extract, FileName, Path}, State) ->
  extract:protocol(FileName, Path),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
