-module(build_fsm).

-export([to_fsm/1,tograph/1]).

-include("reng.hrl").

standard_state() -> standard_state.
choice_state() -> choice_state.
end_state() -> end_state.

to_fsm(P) ->
  {Edges, Nodes, _, _, _, _, _} = to_fsm(P, [], maps:new(), maps:new(), 1, 1, -1, []),
  {Edges, Nodes}.

args(Param) ->
  Str = atom_to_list(Param),
  Recv = string:find(Str, "r_"),
  Send = string:find(Str, "s_"),
  if
    Recv =:= Str ->
      Var = lists:last(string:split(Str, "r_")),
      Act = list_to_atom("receive_"++Var);
    Send =:= Str ->
      Var = lists:last(string:split(Str, "s_")),
      Act = list_to_atom("send_" ++ Var);
    true ->
      Var = Str,
      Act = list_to_atom("act_" ++ Var)
   end,
   {Act, string:titlecase(Var)}.

data(Param, Cons) when Cons =/= [] ->
  {Act, Var} = args(Param),
  #data{action = Act, var = list_to_atom(Var), event = cast, cons = Cons};
data(Param, _Cons) ->
  {Act, Var} = args(Param),
  #data{action = Act, var = list_to_atom(Var), event = cast}.



to_fsm({act, Act, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    Index = PrevIndex + 1,
    Edge = #trans{from = PrevVis, to = Index, data = data(Act, Cons)},
    Edges1 = Edges ++ [Edge],
    Nodes1 = maps:put(PrevVis, standard_state(), Nodes),
    to_fsm(P, Edges1, Nodes1, RecMap, Index, Index, EndIndex, Cons);

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

% post-condition
to_fsm({assert, _N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons);
% pre-condition
to_fsm({require, _N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    to_fsm(P, Edges, Nodes,  RecMap, PrevIndex, PrevVis, EndIndex, Cons);
% pre-condition
to_fsm({consume, _N, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons);

to_fsm({_, _, P}, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons) ->
    to_fsm(P, Edges, Nodes, RecMap, PrevIndex, PrevVis, EndIndex, Cons).


get_vertex(Graph, Label) ->
    Vertices = digraph:vertices(Graph),
    find_label(Label, Vertices, Graph).

find_label(Label, [V|Rest], Graph) ->
    case digraph:vertex(Graph, V) of
      {V, Label} ->
        V;
      _ ->
        find_label(Label, Rest, Graph)
    end;
find_label(_, [], _) ->
    {error, no_label}.

tograph(P) ->
    Graph = digraph:new(),
    {Edges, Nodes,  _, _,  _, _} = to_fsm(P),
    io:format("~p~n", [Edges]),
    lists:foreach(fun(Vertex) ->
                    V = digraph:add_vertex(Graph),
                    digraph:add_vertex(Graph, V, Vertex)
                  end, maps:keys(Nodes)),
    lists:foreach(fun(#trans{from = From, to = To, data = Data}) ->
                    digraph:add_trans(Graph, get_vertex(Graph, From),
                                     get_vertex(Graph,To), Data);
                     (_) -> ok
                  end, Edges),
    {ok, File} = file:open("output", [write]),
    io:format(File, "~s~n", [dot:digraph_to_dot("test", Graph)]),
    file:close(File),
    Graph.
