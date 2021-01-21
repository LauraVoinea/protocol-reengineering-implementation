-module(generate).

-export([gen/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

-import(merl, [term/1]).

endState() ->
  Clause = ?Q(["(_Reason, _State, _Data) -> ok"]),
  {true, terminate, [Clause]}.

stdState(Id, [Edge], Nodes) ->
  case maps:get(Edge#trans.to, Nodes) of
    end_state -> NextState = normal,
                 Trans = stop;
    _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#trans.to)),
             Trans = next_state
  end,
  Act = Edge#trans.data#data.action,
  Var = merl:var(Edge#trans.data#data.var),
  Event = Edge#trans.data#data.event,
  Clause = ?Q(["('@Event@', {'@Act@', _@Var}, Data) ->",
             " {'@Trans@', '@NextState@', Data }"]),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, [Clause]}.

choiceState(Id, Edges, Nodes) ->
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
    ?Q(["('@Event@', {'@Act@', _@Var}, Data) ->",
               " {'@Trans@', '@NextState@', Data }"])
    end,
  Clauses = lists:map(Fun, Edges),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, Clauses}.


stateFuns(K, V, Edges, Nodes) ->
  case V of
    end_state -> endState();
    choice_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Branches = lists:filter(Pred, Edges),
      choiceState(K, Branches, Nodes);
    standard_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Edge = lists:filter(Pred, Edges),
      stdState(K, Edge, Nodes)
  end.


cbFun(#data{action = Act, var = Var, event = Event}, NameMacro) ->
  Var1 = merl:var(Var),
  Clauses = ?Q(["(_@Var1) ->",
             " gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
  {true, Act, [Clauses]}.

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

  {Edges, Nodes} = build_fsm:tofsm(P),
  StateFuns = maps:fold(fun(K, V, AccIn) ->
                AccIn ++ [stateFuns(K, V, Edges, Nodes)] end, [], Nodes),
  CBFuns = lists:foldl(fun(Edge,AccIn) ->
            AccIn ++ [cbFun(Edge#trans.data, Server)] end, [], Edges),
  Fs = [{true, start_link, [Start]},
        {true, callback_mode, [Cb]},
        {true, init, [Init]}
        | StateFuns ] ++ CBFuns ++ [{true, stop, [Stop]}],
  Forms = merl_build:add_attribute(behaviour, 'gen_statem',
            merl_build:init_module(Filename)),
  % Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),
  merl_build:module_forms(
          lists:foldl(fun ({X, Name, Cs}, S) ->
                              merl_build:add_function(X, Name, Cs, S)
                      end,
                      Forms,
                      Fs)).

gen(Filename, P) ->
    Forms = gen_module(Filename, P),
    file:write_file(lists:concat([Filename, ".erl"]),
                    erl_prettypr:format(erl_syntax:form_list(Forms),
                                        [{paper,160},{ribbon,80}])).
