-module(generate).

-compile(export_all).
-record(transition, {to, label}).


standard_state() -> standard_state.
choice_state() -> choice_state.
end_state() -> end_state.


add_transition(FromID, Transition, Transitions) ->
    Values = maps:get(FromID, Transitions, []),
    NewValues = Values ++ [Transition],
    maps:put(FromID, NewValues, Transitions).


tofsm(P) -> tofsm(P, maps:new(), maps:new(), maps:new(), 0, -1, 0, "init").

tofsm({act, Act, P}, S, T, RecMap, PrevIndex, EndIndex, _PrevVis, Label) ->
    S1 = maps:put(PrevIndex + 1, standard_state(), S),
    Index = PrevIndex + 1,
    Transition = #transition{to = Index, label = Label},
    T1 = add_transition(PrevIndex, Transition, T),
    Label1 =  string:uppercase(atom_to_list(Act)),
    tofsm(P, S1, T1, RecMap, Index, EndIndex, Index, Label1);

tofsm({branch, Branches}, S, T, RecMap, PrevIndex, EndIndex, _PrevVis, Label) ->
    Index = PrevIndex + 1,
    S1 = maps:put(Index, choice_state(), S),
    Transition = #transition{to = Index, label = Label},
    T1 = add_transition(PrevIndex, Transition, T),
    tofsmBranches(Branches, S1, T1, RecMap, Index, EndIndex, Index);

tofsm({rec, BoundVar, P}, S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label) ->
    NewRecMap = maps:put(BoundVar, PrevIndex, RecMap),
    tofsm(P, S, T, NewRecMap, PrevIndex, EndIndex, PrevVis, Label);

tofsm({rvar, Var}, S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label) ->
    {ok, NextStateIndex} = maps:find(Var, RecMap),
    Transition = #transition{to = NextStateIndex, label = Label},
    if
      PrevVis =:=  PrevIndex ->
        T1 = add_transition(PrevIndex, Transition, T);
      true ->
        T1 = add_transition(PrevVis, Transition, T)
    end,
    {S, T1, RecMap, PrevIndex, EndIndex, PrevVis, Label};

tofsm(endP, S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label) ->
    IsKey = maps:is_key(EndIndex, S),
    if
      IsKey ->
        {S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label};
      true -> Index = PrevIndex + 1,
              Transition = #transition{to = "stop", label = Label},
              T1 = add_transition(PrevIndex, Transition, T),
              {maps:put(Index, end_state(), S), T1, RecMap, Index, Index, Index, Label}
    end;

tofsm({_, _, P}, S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label) ->
    tofsm(P, S, T, RecMap, PrevIndex, EndIndex, PrevVis, Label).

tofsmBranch({Label, P}, S, T, RecMap, PrevIndex, EndIndex, _PrevVis) ->
    Label1 =  string:uppercase(atom_to_list(Label)),
    Pred = fun(_,V) -> V =:= choice_state() end,
    BrIndex = lists:last(maps:keys(maps:filter(Pred, S))),
    tofsm(P, S, T, RecMap, PrevIndex, EndIndex, BrIndex, Label1).

tofsmBranches([], S, T, RecMap, PrevIndex, EndIndex, PrevVis) ->
  {S, T, RecMap, PrevIndex, EndIndex, PrevVis};
tofsmBranches([P], S, T, RecMap, PrevIndex, EndIndex, PrevVis) ->
    tofsmBranch(P, S, T, RecMap, PrevIndex, EndIndex, PrevVis);
tofsmBranches([P | PS], S, T, RecMap, PrevIndex, EndIndex, PrevVis) ->
    {NewStates, NewT, NewRecMap, NewPrevIndex, NewEndIndex, PrevVis1, _Label1} =
      tofsmBranch(P, S, T, RecMap, PrevIndex, EndIndex, PrevVis),
    tofsmBranches(PS, NewStates, NewT, NewRecMap, NewPrevIndex, NewEndIndex, PrevVis1).

gen(File, P) ->
    {ok, IODevice} = file:open(File, [write]),
    LineSep = io_lib:nl(),
    [Name, _] = string:tokens(File, "."),
    io:format(IODevice, "-module(~s). ~s", [Name, LineSep]),
    io:format(IODevice,
              "-behaviour(gen_statem). ~s",
              [LineSep]),
    io:format(IODevice,
              "-define(NAME, ~s). ~s~n",
              [Name, LineSep]),

    io:format(IODevice,
              "~s ~n~n",
              ["-export([start_link/1, stop/0])."]),
    io:format(IODevice, "~s ~n~n", ["-export([init/1])."]),

    {S, T, _, _, _, _, _} = tofsm(P),
    Transitions = maps:remove(0, T),
    io:format(IODevice, "~s ~n~n", [pprintStatesToExport(S)]),
    io:format(IODevice, "~s ~n~n", [pprintStart()]),

    lists:foreach(fun({K, V}) ->
            case maps:get(K, S) of
              standard_state -> io:format(IODevice, "~s ~n", [pprintFunStd(V)]),
                                io:format(IODevice, "~s ~n", [pprintState({K, V})]);
              choice_state -> io:format(IODevice, "~s ~n", [pprintChoice({K, V})]);
              end_state -> io:format(IODevice, "~s ~n", [pprintStop()])
            end
    end, maps:to_list(Transitions)),
    io:format(IODevice, "~s ~n", [pprintStop()]).


pprintStart() ->
    "start_link([]) -> \n \t gen_statem:start_link"
    "({local,?NAME}, ?MODULE, [], []). \n\n"
        ++
        "init([]) -> \n\t {ok, state0, []}. \n\n" ++
            "callback_mode() -> \n \t state_functions. ".

pprintStatesToExport(S) ->
  List = lists:map(fun({Key, Val}) ->
                case Val of
                  standard_state -> "state" ++ integer_to_list(Key) ++ "/3";
                  choice_state -> "choice" ++ integer_to_list(Key) ++ "/3";
                  end_state -> "terminate" ++ "/3"
                end
              end,  maps:to_list(S)),

  "-export([" ++ lists:flatten(lists:join(", ", List)) ++ "]).".

pprintStop() ->
    "stop() -> \n \t gen_statem:stop(?NAME).\n".

pprintState({Key, [Value]}) ->
    "state" ++ integer_to_list(Key) ++
    "({call, From}, {act"++ Value#transition.label ++", " ++ Value#transition.label ++  "}, Data) -> \n" ++
    pprintNextState(Value) ++ ".".

pprintChoice({Key, Value}) ->
      Ch = lists:map(fun(T) ->
        "choice" ++ integer_to_list(Key) ++
        "({call, From}, {choice"++ T#transition.label ++", " ++ T#transition.label ++  "}, Data) -> \n"
      ++ pprintNextState(T)
    end, Value),
      lists:flatten(lists:join("; \n", Ch)) ++ ".\n"
      .

pprintNextState(T) when is_number(T#transition.to) ->
  "\t {next_state, state" ++ integer_to_list(T#transition.to) ++ ", Data}";
pprintNextState(T) ->
  "\t {next_state, " ++  T#transition.to ++ ", Data}".
pprintNextStop() -> "\t {next_state, stop, {}}. \n".

pprintFunStd([T]) ->
  "act" ++ T#transition.label ++ "(" ++ T#transition.label ++ ") ->
    gen_statem:call(?NAME, {act"++ T#transition.label ++", " ++ T#transition.label ++ "})."
    .
pprintFunCh(T) ->
  "choice" ++ T#transition.label ++ "(" ++ T#transition.label ++ ") ->
    gen_statem:call(?NAME, {choice"++ T#transition.label ++", " ++ T#transition.label ++ "})."
    .

example2() ->
  {rec, "x", {act, a, {branch, [{l, {act, b, endP}}
                               ,{r, {rvar, "x"}}]}}}.
