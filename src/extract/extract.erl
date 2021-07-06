-module(extract).

-export([protocol/1]).
-include("reng.hrl").

-spec protocol(string())-> interleave:protocol().
protocol(File) ->
  {parsed, gen_statem, Graph} = build_graph:parse_file(File),
  Vs = lists:sort(digraph:vertices(Graph)),
  % V = hd(lists:delete(hd(Vs), Vs)),
  V = hd(Vs),
  rec(V, Graph, maps:new(), maps:new(), digraph:get_cycle(Graph, V)).

cons({}, Protocol) -> Protocol;
cons({_, _, _, Text}, Protocol) ->
  Fun = fun(Elem, P) -> Es = string:tokens(Elem, " ()"),
                        case Es of 
                          [] -> P;
                          [_One] -> P;
                          ["require", Var] -> {require, list_to_atom(Var), P};
                          ["assert", Var] -> {assert, list_to_atom(Var), P};
                          ["consume", Var] -> {consume, list_to_atom(Var), P};
                          [_One, _Two, _Three | _Tail] -> P
                        end
        end,
  lists:foldr(Fun, Protocol, Text).

rec(V, G, Cs, RecMap, Cyc) when is_list(Cyc) ->
  In = digraph:in_degree(G, V),
  OutEdges = digraph:out_edges(G, V),
  {_E, _V1, _V2, Label} = digraph:edge(G, hd(OutEdges)),
  case maps:is_key(lists:usort(Cyc), Cs) of
    true ->  rvar(V, G, digraph:out_degree(G, V), Cs, RecMap);
    false when In > 1 -> {_, VLab} = digraph:vertex(G, V),
            RecLab = atom_to_list(VLab),
            Cs1 = maps:put(lists:usort(Cyc), lists:last(Cyc), Cs),
            RecMap1 = maps:put(lists:last(Cyc), RecLab, RecMap),
            {'rec', RecLab, acts(V, G, digraph:out_degree(G, V), Cs1, RecMap1)};
    false -> 
      cons(Label#edge_data.comments, acts(V, G, digraph:out_degree(G, V), Cs, RecMap))     
  end;
rec(V, G, Cs, RecMap, _Cyc) -> 
  case  digraph:in_degree(G, V) == 0 of
    true ->   OutEdges = digraph:out_edges(G, V),
               {_E, _V1, V2, Label} = digraph:edge(G, hd(OutEdges)),
               cons(Label#edge_data.comments, rec(V2, G, Cs, RecMap, digraph:get_cycle(G, V2)));
    false ->   acts(V, G, digraph:out_degree(G, V), Cs, RecMap)
  end. 
  


rvar(V, G, O, Cs, RecMap) ->
  case maps:is_key(V, RecMap) of
    false -> acts(V, G, O, Cs, RecMap);
    true -> {'rvar', maps:get(V, RecMap)}
  end.

acts(_V, _G, O, _Cs, _RecMap) when O == 0 ->
  'endP';
acts(V, G, O, Cs, RecMap) when O == 1 ->
  OutEdges = digraph:out_edges(G, V),
  {_E, _V1, V2, Label} = digraph:edge(G, hd(OutEdges)),
  {'act', var(Label), cons(Label#edge_data.comments, rec(V2, G, Cs, RecMap, digraph:get_cycle(G, V2)))};
acts(V, G, O, Cs, RecMap) when O > 1 ->
  OutEdges = digraph:out_edges(G, V),
  {'branch', lists:map(fun(Edge) ->
                {_E, _V1, V2, Label} = digraph:edge(G, Edge),
                {var(Label), cons(Label#edge_data.comments, rec(V2, G, Cs, RecMap, digraph:get_cycle(G, V2)))}
              end, OutEdges)}.

var(Label) ->
  [L] = Label#edge_data.event,
  list_to_atom(string:lowercase(hd(lists:last(lists:last(L))))).
