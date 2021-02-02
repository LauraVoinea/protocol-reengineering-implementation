-module(generate).

-export([gen/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

end_state() ->
  Clause = ?Q(["(_Reason, _State, _Data) -> ok"]),
  {true, terminate, [Clause]}.

clause(Event, Act, Var, Trans, NextState, Cons) when length(Cons) == 0 ->
  ?Q(["('@Event@', {'@Act@', _@Var}, Data) ->",
         " {'@Trans@', '@NextState@', Data }"]);
clause(Event, Act, V, Trans, NextState, Cons) ->
  Fun = fun({require, Var}, Body) ->
          % Body1 = lists:flatten(Body),
          % io:format("require ~p ~n", []),
          % merl:show(Body1),
          ?Q(["case require(Data) of",
                      "true -> _@Body;",
                      "false -> {keep_state_and_data}",
                      "end"]),
          ?Q(["case require(Data) of",
                      "true -> _@Body;",
                      "false -> {keep_state_and_data}",
                      "end"]);
            ({assert, Var}, Body) ->
              % Body1 = lists:flatten(Body),
              % io:format("Assert ~p ~n", [Body1]),
%
             ?Q(["case assert(Data) of",
                        "true -> _@Body;",
                        "false -> {keep_state_and_data}",
                        "end"]),
            ?Q(["case assert(Data) of",
                       "true -> _@Body;",
                       "false -> {keep_state_and_data}",
                       "end"]);
            ({consume, Var}, Body) ->
              % Body1 = lists:flatten(Body),
              % io:format("Consume ~p ~n", [Body1]),
             ?Q(["case consume(Data) of",
                        "true -> _@Body;",
                        "false -> {keep_state_and_data}",
                        "end"]),
            ?Q(["case consume(Data) of",
                       "true -> _@Body;",
                       "false -> {keep_state_and_data}",
                       "end"])
          end,

  % /C = ?Q(["{'@Trans@', '@NextState@', Data }"]),
  Body1 = lists:last(lists:mapfoldl(Fun, "", Cons)),
  % merl:show(Body),
  % io:format("~p ~n", [Body1]),
  ?Q(["('@Event@', {'@Act@', _@V}, Data) -> _@Body1 "]).

std_state(Id, [Edge], Nodes) ->
  case maps:get(Edge#trans.to, Nodes) of
    end_state -> NextState = normal,
                 Trans = stop;
    _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#trans.to)),
             Trans = next_state
  end,
  Act = Edge#trans.data#data.action,
  Var = merl:var(Edge#trans.data#data.var),
  Event = Edge#trans.data#data.event,
  Cons = Edge#trans.data#data.cons,
  Clause = clause(Event, Act, Var, Trans, NextState, Cons),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, [Clause]}.

choice_state(Id, Edges, Nodes) ->
  Fun = fun(Edge) ->
    case maps:get(Edge#trans.to, Nodes) of
      end_state -> NextState = normal,
                   Trans = stop;
      _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#trans.to)),
               Trans = next_state
    end,
    Act = Edge#trans.data#data.action,
    Var = merl:var(Edge#trans.data#data.var),
    Event = Edge#trans.data#data.event,
    Cons = Edge#trans.data#data.cons,
    clause(Event, Act, Var, Trans, NextState, Cons)
    end,
  Clauses = lists:map(Fun, Edges),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, Clauses}.


state_funs(K, V, Edges, Nodes) ->
  case V of
    end_state -> end_state();
    choice_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Branches = lists:filter(Pred, Edges),
      choice_state(K, Branches, Nodes);
    standard_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Edge = lists:filter(Pred, Edges),
      std_state(K, Edge, Nodes)
  end.


cb_fun(#data{action = Act, var = Var, event = Event}, NameMacro) ->
  Var1 = merl:var(Var),
  Clauses = ?Q(["(_@Var1) ->",
             " gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
  {true, Act, [Clauses]}.

cons_funs(#data{cons = Cons}) ->
  lists:map(fun({Con, Var}) ->
            Name = list_to_atom(atom_to_list(Con) ++ "_" ++ atom_to_list(Var)),
            Clauses = ?Q(["(Data) ->",
                       "true"]),
            {true, Name, [Clauses]}
          end, Cons).

gen_module(Filename, P) ->
  Server = merl:var(list_to_atom("?SERVER")),
  Module = merl:var(list_to_atom("?MODULE")),

  Start = ?Q(["() -> ",
           "gen_statem:start_link({local, _@Server}, _@Module, [], []) "]),
  Cb = ?Q(["() -> ",
           "state_functions "]),
  Stop = ?Q(["() -> ",
            "gen_statem:stop(_@Server)"]),
  Init = ?Q(["([]) ->
               {ok, state1, {}}
            "]),

  {Edges, Nodes} = build_fsm:to_fsm(P),
  StateFuns = maps:fold(fun(K, V, AccIn) ->
                AccIn ++ [state_funs(K, V, Edges, Nodes)] end, [], Nodes),
  CBFuns = lists:foldl(fun(Edge,AccIn) ->
            AccIn ++ [cb_fun(Edge#trans.data, Server)] end, [], Edges),
  ConsFuns = lists:foldl(fun(Edge,AccIn) ->
          AccIn ++ cons_funs(Edge#trans.data) end, [], Edges),
  Fs = [{true, start_link, [Start]},
        {true, callback_mode, [Cb]},
        {true, init, [Init]}
        | StateFuns ] ++ CBFuns ++ [{true, stop, [Stop]}] ++ ConsFuns,
  Forms = merl_build:add_attribute(behaviour, [merl:term('gen_statem')],
            merl_build:init_module(Filename)),
  Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),
  merl_build:module_forms(
          lists:foldl(fun ({X, Name, Cs}, S) ->
                              merl_build:add_function(X, Name, Cs, S)
                      end,
                      Forms1,
                      Fs)).

gen(Filename, P) ->
    Forms = gen_module(Filename, P),
    file:write_file(lists:concat([Filename, ".erl"]),
                    erl_prettypr:format(erl_syntax:form_list(Forms),
                                        [{paper,160},{ribbon,80}])).
