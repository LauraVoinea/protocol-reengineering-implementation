%% @doc Process a protocol structure into fsm edges and nodes
-module(build_fsm).

-export([to_fsm/1]).

-include("reng.hrl").

standard_state() -> standard_state.
choice_state() -> choice_state.
end_state() -> end_state.
%% @doc wrapper function that initializes the main to_fsm function
%% takes a protocol and returns a list of transitions/edges and nodes/states
-spec to_fsm(interleave:protocol()) -> {list(), map()}.
to_fsm(P) ->
  {Edges, Nodes, _, _, _, _, _} = to_fsm(P, [], maps:new(), maps:new(), 1, 1, -1, []),
  {Edges, Nodes}.

%% @doc processes the actions and labels names and sets the event,
%% variable name and action accordingly. When the name starts with r_ that
%% represents a receive action, with a s_ it represents a send action
-spec args(atom()) -> {atom(), string(), atom()}.
args(Param) ->
  Str = atom_to_list(Param),
  Recv = string:find(Str, "r_"),
  Send = string:find(Str, "s_"),
  if
    Recv =:= Str ->
      Event = cast,
      Var = lists:last(string:split(Str, "r_")),
      Act = list_to_atom("receive_"++Var);
    Send =:= Str ->
      Event = internal,
      Var = lists:last(string:split(Str, "s_")),
      Act = list_to_atom("send_" ++ Var);
    true ->
      Event = cast,
      Var = Str,
      Act = list_to_atom("act_" ++ Var)
   end,
   {Act, string:titlecase(Var), Event}.

%% @doc Checks whether there are any constraints; adds them to the trans record
%% calls args.
-spec data(atom(), list()) -> {atom(), string(), atom()}.
data(Param, Cons) when Cons =/= [] ->
  {Act, Var, Event} = args(Param),
  #data{action = Act, var = list_to_atom(Var), event = Event, cons = Cons};
data(Param, _Cons) ->
  {Act, Var, Event} = args(Param),
  #data{action = Act, var = list_to_atom(Var), event = Event}.

%% @doc transform the protocol to a list of transitions between states and a
%% map of nodes, easier to work with in generate
-spec to_fsm(interleave:protocol(), list(), map(), map(), integer(), integer(),
  integer(), list()) -> {list(), map(), map(), integer(), integer(), integer(), list()}.
to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Index = PrevIndex + 1,
    Edge = #trans{from = PrevVis, to = Index, data = data(Act, Cons)},
    Edges1 = Edges ++ [Edge],
    Nodes1 = maps:put(PrevVis, standard_state(), Nodes),
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, []);
to_fsm({branch, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    lists:foldl(fun({Label, P1}, {E, N, R, I, _, EI, C}) ->
      I1 = I + 1,
      Edge = #trans{from = PrevVis, to = I1, data = data(Label, C)},
      E1 = E ++ [Edge],
      to_fsm(P1, E1, N, R, I1, I1, EI, []) end,
      {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex, Cons}, Branches);
to_fsm({rec, BoundVar, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    RecMap1 = maps:put(BoundVar, PrevVis, RecMap),
    to_fsm(P, Edges, Nodes, RecMap1, PrevIndex, PrevVis, EndIndex, Cons);
to_fsm({rvar, Var}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    RecIndex = maps:get(Var, RecMap),
    LastEdge = lists:last(Edges),
    Edge = LastEdge#trans{to=RecIndex},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons};
to_fsm(endP, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    if
      EndIndex =:= -1 ->
        Nodes1 = maps:put(PrevVis, end_state(), Nodes),
        {Edges, Nodes1, RecMap, PrevIndex, PrevVis, PrevVis, Cons};
      EndIndex =/= -1 ->
         LastEdge = lists:last(Edges),
         Edge = LastEdge#trans{to = EndIndex},
         Edges1 = lists:droplast(Edges) ++ [Edge],
        {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons}
    end;
to_fsm({assert, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Cons1 = Cons ++ [{assert, N}],
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons1);
to_fsm({require, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Cons1 = Cons ++ [{require, N}],
    to_fsm(P, Edges, Nodes,  RecMap, PrevIndex, PrevVis, EndIndex, Cons1);
to_fsm({consume, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Cons1 = Cons ++ [{consume, N}],
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons1);
to_fsm({_, _, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons).
