-module(generate).

-export([tofsm/1, gen/2]).

-include_lib("syntax_tools/include/merl.hrl").
-import(merl, [term/1]).


-record(trans, {action :: atom(),
                var :: atom(),
                next
                }).

-record(state, {type :: atom(),
                transitions = [#trans{}] :: list(),
                cons = []:: list()
                }).

standard_state() -> standard_state.
choice_state() -> choice_state.
end_state() -> end_state.

trans(Str) ->
  Rec = string:find(Str, "r_"),
  Send = string:find(Str, "s_"),
  if
    Rec =:= Str ->
      [#trans{action = 'receive', var = lists:last(string:split(Str, "r_"))}];
    Send =:= Str ->
      [#trans{action = 'send', var = lists:last(string:split(Str, "s_"))}];
    true -> [#trans{action = act, var = Str}]
  end.

updateNextIndex(PrevIndex, Index, StateMap) ->
  Fun = fun(V) ->
    Trans = lists:last(V#state.transitions),
    NTrans = Trans#trans{next=Index},
    V#state{transitions = lists:droplast(V#state.transitions) ++ [NTrans]}
  end,
  maps:update_with(PrevIndex, Fun, StateMap).

-spec tofsm(interleave:protocol()) -> map().
tofsm(P) ->
    T = [#trans{action = init, var = "init", next = 1}],
    State = #state{type = standard_state(), transitions = T},
    StateMap = maps:put(0, State, maps:new()),
    {StateMap1, _, _, _, _ } = tofsm(P, StateMap, maps:new(), 0, -1, 0),
    StateMap1.

-spec tofsm(interleave:protocol(), map(), map(), integer(), integer(), integer()) -> tuple().
tofsm({act, Act, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    Index = PrevIndex + 1,
    StateMap1 = updateNextIndex(PrevVis, Index, StateMap),
    T = trans(atom_to_list(Act)),
    State = #state{type = standard_state(), transitions = T},
    StateMap2 = maps:put(Index, State, StateMap1),
    tofsm(P, StateMap2, RecMap, Index, EndIndex, Index);

tofsm({branch, Branches}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    Index = PrevIndex + 1,
    State = #state{type = choice_state(), transitions = []},
    StateMap1 = maps:put(Index, State, updateNextIndex(PrevVis, Index, StateMap)),
    lists:foldl(fun({Label, P1}, {S, R, I, E, _}) ->
      Fun = fun(V) -> V#state{transitions = V#state.transitions ++
        trans(atom_to_list(Label))} end,
      S1 = maps:update_with(Index, Fun, S),
      tofsm(P1, S1, R, I, E, Index) end,
      {StateMap1, RecMap, Index, EndIndex, Index}, Branches);

tofsm({rec, BoundVar, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    RecMap1 = maps:put(BoundVar, PrevVis + 1, RecMap),
    tofsm(P, StateMap, RecMap1, PrevIndex, EndIndex, PrevVis);

tofsm({rvar, Var}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    RecIndex = maps:get(Var, RecMap),
    StateMap1 = updateNextIndex(PrevVis, RecIndex, StateMap),
    {StateMap1, RecMap, PrevIndex, EndIndex, PrevVis};

tofsm(endP, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    IsKey = maps:is_key(EndIndex, StateMap),
    if
      IsKey ->
              {updateNextIndex(PrevVis, EndIndex, StateMap), RecMap, EndIndex, EndIndex, EndIndex};
      true -> Index = PrevIndex + 1,
              State = #state{type = end_state(), transitions = [#trans{action = endP, var = "", next = ""}]},
              StateMap1 = maps:put(Index, State, updateNextIndex(PrevVis, Index, StateMap)),
              {StateMap1, RecMap, Index, Index, Index}
    end;

% post-condition
tofsm({assert, N, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    Fun = fun(V) -> V#state{cons = V#state.cons ++ [{assert, N}]} end,
    StateMap1 = maps:update_with(PrevVis, Fun, StateMap),
    tofsm(P, StateMap1, RecMap, PrevIndex, EndIndex, PrevVis);
% pre-condition
tofsm({require, N, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    Fun = fun(V) -> V#state{cons = V#state.cons ++ [{require, N}]} end,
    StateMap1 = maps:update_with(PrevVis, Fun, StateMap),
    tofsm(P, StateMap1, RecMap, PrevIndex, EndIndex, PrevVis);
% pre-condition
tofsm({consume, N, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    Fun = fun(V) -> V#state{cons = V#state.cons ++ [{consume, N}]} end,
    StateMap1 = maps:update_with(PrevVis, Fun, StateMap),
    tofsm(P, StateMap1, RecMap, PrevIndex, EndIndex, PrevVis);

tofsm({_, _, P}, StateMap, RecMap, PrevIndex, EndIndex, PrevVis) ->
    tofsm(P, StateMap, RecMap, PrevIndex, EndIndex, PrevVis).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_sfun(K, {state, standard_state, [T], _}) ->
  Next = list_to_atom("state" ++ integer_to_list(T#trans.next)),
  Act = T#trans.action,
  Var = merl:var(list_to_atom(string:uppercase(T#trans.var))),
  Clause = ?Q(["({call, From}, {'@Act@', _@Var}, Data) ->",
             " {next_state, '@Next@', Data }"
            ]),

  Name = list_to_atom("state" ++ integer_to_list(K)),
  {true, Name, [Clause]};

gen_sfun(K, {state, end_state, _, _}) ->
  Next = stop,
  Param = endP,
  Clause = ?Q(["({call, From}, {'@Param@'}, Data) ->",
             " {next_state, '@Next@', Data }"
            ]),
  Name = list_to_atom("state" ++ integer_to_list(K)),
  {true, Name, [Clause]};

gen_sfun(K, {state, choice_state, Ts, _}) ->
  Fun = fun(T) ->  Next = list_to_atom("state" ++ integer_to_list(T#trans.next)),
                  Act = T#trans.action,
                  Var = merl:var(list_to_atom(string:uppercase(T#trans.var))),
                  ?Q(["({call, From}, {'@Act@', _@Var}, Data) ->",
                             " {next_state, '@Next@', Data }"
                            ])
                  end,
  Clauses = lists:map(Fun, Ts),
  Name = list_to_atom("state" ++ integer_to_list(K)),
  {true, Name, Clauses}.

gen_cbfun({_, _, Ts, _}, NameMacro) ->
  First =  hd(Ts),
  Name = First#trans.action,
  Fun = fun(T) -> Act = T#trans.action,
                  Var = merl:var(list_to_atom(string:uppercase(T#trans.var))),
                  ?Q(["(_@Var) ->",
                             " gen_statem:call(_@NameMacro, {'@Act@',  _@Var})"
                            ])
                  end,
  Clauses = lists:map(Fun, Ts),
  {true, Name, Clauses}.

gen_module(Filename, P) ->
  NameMacro = merl:var(list_to_atom("?NAME")),
  Start = ?Q(["([]) -> ",
              "gen_statem:start_link({local, _@NameMacro}, MODULE, [], []) "]),
  Cb = ?Q(["() -> ",
           "state_functions "]),
  Stop = ?Q(["() -> ",
            "gen_statem:stop(_@NameMacro)"]),

  Init = ?Q(["([]) ->
               {ok, state1, Data}
            "]),

  StateMap = tofsm(P),
  StateFuns = maps:fold(fun(K,V,AccIn) -> AccIn ++ [gen_sfun(K, V)] end,
                        [], maps:without([0], StateMap)),

  Fun = fun(K,V,AccIn) -> AccIn ++ [gen_cbfun(V, NameMacro)] end,
  CBFuns = maps:fold(Fun, [], maps:without([0], StateMap)),

  Fs = [{true, start_link, [Start]},
        {true, callback_mode, [Cb]},
        {true, init, [Init]}
        | StateFuns ] ++ CBFuns ++ [{true, stop, [Stop]}],

  Forms = merl_build:add_attribute(behaviour, gen_statem, merl_build:init_module(Filename)),
  merl_build:module_forms(
          lists:foldl(fun ({X, Name, Cs}, S) ->
                              merl_build:add_function(X, Name, Cs, S)
                      end,
                      Forms,
                      Fs)).

gen(Filename, P) ->
    Forms = gen_module(Filename, P),
    file:write_file(lists:concat([Filename, "_gen.erl"]),
                    erl_prettypr:format(erl_syntax:form_list(Forms),
                                        [{paper,160},{ribbon,80}])).
