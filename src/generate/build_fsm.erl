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
  Edge = #edge{from = 0, to = 1, edge_data = #edge_data{event = init, event_type = init}},
  {Edges, Nodes, _, _, _, _} = to_fsm(P, [Edge], maps:put(0, init_state, maps:new()), maps:new(), 1, 1, -1),
  % {Edges, Nodes, _, _, _, _, _} = to_fsm(P, [], maps:new(), maps:new(), 1, 1, -1, []),
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
      Act = list_to_atom("receive_"++ Var);
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
-spec data(atom()) -> {atom(), string(), atom()}.
data(Param) ->
  {Act, Var, Event} = args(Param),
  #edge_data{event = {Act, list_to_atom(Var)}, event_type = Event}.

%% @doc transform the protocol to a list of transitions between states and a
%% map of nodes, easier to work with in generate
-spec to_fsm(interleave:protocol(), list(), map(), map(), integer(), integer(),
  integer()) -> {list(), map(), map(), integer(), integer(), integer()}.
to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    Index = PrevIndex + 1,
    Edge = #edge{from = PrevVis, to = Index, edge_data = data(Act)},
    Edges1 = Edges ++ [Edge],
    Nodes1 = maps:put(PrevVis, standard_state(), Nodes),
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex);
to_fsm({branch, Branches}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    Index = PrevIndex + 1,
    Nodes1 = maps:put(PrevVis, choice_state(), Nodes),
    lists:foldl(fun({Label, P1}, {E, N, R, I, _, EI}) ->
      I1 = I + 1,
      Edge = #edge{from = PrevVis, to = I1, edge_data = data(Label)},
      E1 = E ++ [Edge],
      to_fsm(P1, E1, N, R, I1, I1, EI) end,
      {Edges, Nodes1, RecMap, PrevIndex, Index, EndIndex}, Branches);
to_fsm({rec, BoundVar, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    RecMap1 = maps:put(BoundVar, PrevVis, RecMap),
    to_fsm(P, Edges, Nodes, RecMap1, PrevIndex, PrevVis, EndIndex);
to_fsm({rvar, Var}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    RecIndex = maps:get(Var, RecMap),
    LastEdge = lists:last(Edges),
    Edge = LastEdge#edge{to=RecIndex},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex};
to_fsm(endP, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    if
      EndIndex =:= -1 ->
        Nodes1 = maps:put(PrevVis, end_state(), Nodes),
        {Edges, Nodes1, RecMap, PrevIndex, PrevVis, PrevVis};
      EndIndex =/= -1 ->
         LastEdge = lists:last(Edges),
         Edge = LastEdge#edge{to = EndIndex},
         Edges1 = lists:droplast(Edges) ++ [Edge],
        {Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex}
    end;
to_fsm({assert, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{assert, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},
    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex);
to_fsm({require, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{require, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes,  RecMap, PrevIndex, PrevVis, EndIndex);
to_fsm({consume, N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    LastEdge = lists:last(Edges),
    EdgeData = LastEdge#edge.edge_data#edge_data{comments = LastEdge#edge.edge_data#edge_data.comments ++ [{consume, N}]},
    Edge = LastEdge#edge{edge_data = EdgeData},    Edges1 = lists:droplast(Edges) ++ [Edge],
    to_fsm(P, Edges1, Nodes, RecMap, PrevIndex, PrevVis, EndIndex);
to_fsm({_, _, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex).
