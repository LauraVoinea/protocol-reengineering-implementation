-module(extract).

-export([protocol/2]).
-include("reng.hrl").

-spec parse_file(string(), string())-> interleave:protocol().
protocol(File, IncludePaths) ->
  {parsed, gen_statem, Graph} = build_graph:parse_file(File, IncludePaths),
  Vs = lists:sort(digraph:vertices(Graph)),
  V = hd(lists:delete(hd(Vs), Vs)),
  rec(V, Graph, maps:new(), maps:new(), digraph:get_cycle(Graph, V)).

rec(V, G, Cs, RecMap, Cyc) when is_list(Cyc) ->
  In = digraph:in_degree(G, V),
  case maps:is_key(lists:usort(Cyc), Cs) of
    true -> rvar(V, G, digraph:out_degree(G, V), Cs, RecMap);
    false when In > 1 -> {_, VLab} = digraph:vertex(G, V),
            RecLab = atom_to_list(VLab),
            Cs1 = maps:put(lists:usort(Cyc), lists:last(Cyc), Cs),
            RecMap1 = maps:put(lists:last(Cyc), RecLab, RecMap),
            {'rec', RecLab, acts(V, G, digraph:out_degree(G, V), Cs1, RecMap1)};
    false -> acts(V, G, digraph:out_degree(G, V), Cs, RecMap)
  end;
rec(V, G, Cs, RecMap, _Cyc) ->
  acts(V, G, digraph:out_degree(G, V), Cs, RecMap).

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
  {'act', var(Label), rec(V2, G, Cs, RecMap, digraph:get_cycle(G, V2))};
acts(V, G, O, Cs, RecMap) when O > 1 ->
  OutEdges = digraph:out_edges(G, V),
  {'branch', lists:map(fun(Edge) ->
                {_E, _V1, V2, Label} = digraph:edge(G, Edge),
                {var(Label), rec(V2, G, Cs, RecMap, digraph:get_cycle(G, V2))}
              end, OutEdges)}.

var(Label) ->
  [L] = Label#edge_data.event,
  list_to_atom(string:lowercase(hd(lists:last(lists:last(L))))).
