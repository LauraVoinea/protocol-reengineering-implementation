-module(build_graph).

-export([parse_file/2]).

-include("reng.hrl").

-spec parse_file(string(), string())-> {atom(), atom(), tuple()}.
parse_file(File, Path) ->
   {ok, ParsedFile} = epp:parse_file(File, Path, []),
   Behaviour = lists:keyfind(behaviour, 3, ParsedFile),
   Behavior = lists:keyfind(behavior, 3, ParsedFile),
   case Behaviour of
          {attribute, _, _, gen_server} -> parse_gen_server(ParsedFile);
          {attribute, _, _, gen_fsm} -> parse_gen_fsm(ParsedFile);
          {attribute, _, _, gen_statem} -> parse_gen_statem(ParsedFile);
          {attribute, _, _, gen_event} -> parse_gen_event(ParsedFile);
          false ->
            case Behavior of
              {attribute, _, _, gen_server} -> parse_gen_server(ParsedFile);
              {attribute, _, _, gen_fsm} -> parse_gen_fsm(ParsedFile);
              {attribute, _, _, gen_statem} -> parse_gen_statem(ParsedFile);
              {attribute, _, _, gen_event} -> parse_gen_event(ParsedFile);
              false -> {error, not_otp}
            end
        end.

parse_gen_server(_TokenList) -> {error, not_supported}.

parse_gen_statem(TokenList) ->
  Type = gen_statem,
  Fun = fun({function, _, init, 1, Clauses}, Acc) ->
              parse_function_add_edges(Clauses, init, [async], Type, Acc);
           ({function, _, handle_event = FnName, 4, Clauses}, Acc) ->
              parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
           ({function, _, FnName, 3, Clauses}, Acc) ->
              parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
           (_Other, {OldNodes, OldEdges, OldAllStates}) ->
              {OldNodes, OldEdges, OldAllStates}
        end,
    Graph = generic_parse(TokenList, Fun),
    {parsed, Type, Graph}.

parse_gen_fsm(TokenList) ->
  Type = gen_fsm,
  Fun =
    fun({function, _, init, 1, Clauses}, Acc) ->
          parse_function_add_edges(Clauses, init, [async], Type, Acc);
       ({function, _, handle_event, 3, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_event, [async, allstate], Type, Acc);
       ({function, _, handle_sync_event, 4, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_sync_event, [sync, allstate], Type, Acc);
       ({function, _, handle_info, 3, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_info, [async, allstate, info], Type, Acc);
       ({function, _, FnName, 2, Clauses}, Acc) ->
          parse_function_add_node_and_edges(Clauses, FnName, [async], Type, Acc);
       ({function, _, FnName, 3, Clauses}, Acc) ->
          parse_function_add_node_and_edges(Clauses, FnName, [sync], Type, Acc);
       (_Other, {OldNodes, OldEdges, OldAllStates}) ->
            {OldNodes, OldEdges, OldAllStates}
    end,
    Graph = generic_parse(TokenList, Fun),
    {parsed, Type, Graph}.

generic_parse(TokenList, Fun) ->
  Graph = digraph:new(),
  {States, Edges, AllStateEdges} = lists:foldl(Fun, {[init, terminate],[],[]}, TokenList),
  NewAllStateEdges = expand_allstates(AllStateEdges, States),
  lists:foreach(fun(Vertex) ->
                  V = digraph:add_vertex(Graph),
                  digraph:add_vertex(Graph, V, Vertex)
                end, lists:usort(States)),
  lists:foreach(fun(#edge{from = From, to = To, edge_data = Data}) ->
                  digraph:add_edge(Graph, get_vertex(Graph, From),
                                   get_vertex(Graph,To), Data);
                   (_) -> ok
                end, remove_dups(Edges ++ NewAllStateEdges)),
  Graph.

parse_gen_event(_TokenList) ->
  {error, not_supported}.

parse_function_add_states(Clauses, FnName, Options, Type,
                          {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} -> {OldNodes, OldEdges, OldAllStates};
    NewEdges -> {OldNodes, OldEdges, OldAllStates ++ NewEdges}
  end.

parse_function_add_edges(Clauses, FnName, Options, Type,
                          {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} -> {OldNodes, OldEdges, OldAllStates};
    NewEdges -> {OldNodes, OldEdges ++ NewEdges, OldAllStates}
  end.

parse_function_add_node_and_edges(Clauses, FnName, Options, Type,
                                  {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} -> {OldNodes, OldEdges, OldAllStates};
    NewEdges -> {[FnName|OldNodes], OldEdges ++ NewEdges, OldAllStates}
  end.

parse_function(Clauses, FnName, Options, Type) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
               [parse_function_clause(Clause, FnName, Options, Type) | AccIn]
              end, [], Clauses),
  FlatEdges = lists:flatten(Edges),

  case lists:all(fun (El) -> {error, bad_transition} == El end, Edges) of
    false -> FlatEdges;
    true -> {error, not_a_state}
  end.

parse_function_clause({clause, _Line, Args, Guards, Body},
                      init, _Options, Type) ->
  Fun = fun({ok, NextState, init}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            from = init,
            to = NextState,
            edge_data =
            #edge_data{
              event = "",
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [async]}
          }
        end,
  map_parse_func(Fun, init, Body, Type);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      handle_info, Options, gen_fsm) ->
  Fun = fun({ok, NextState, RetType}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            from = handle_info,
            to = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [RetType|Options]}
          }
        end,
  map_parse_func(Fun, handle_info, Body, gen_fsm);
parse_function_clause({clause, _Line, [_EventType, State, Event | Args], Guards, Body},
                      handle_event, Options, gen_statem) ->
  Fun = fun({ok, NextState, RetType}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            from = State,
            to = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [RetType|Options]}
          }
        end,
  map_parse_func(Fun, handle_info, Body, gen_fsm);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      FnName, Options, gen_fsm) ->
  Fun = fun({ok, NextState, _}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            from = FnName,
            to = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = Options}
          }
        end,
  map_parse_func(Fun, FnName, Body, gen_fsm);
parse_function_clause({clause, _Line, [_Call, Event | Args], Guards, Body},
                      FnName, Options, gen_statem) ->
  Fun = fun({ok, NextState, _}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{from = FnName, to = NextState, edge_data =
                                                        #edge_data{
                                                          event = PrettyEvent,
                                                          args = PrettyArgs,
                                                          pattern = Args,
                                                          guard = PrettyGuards,
                                                          code = PrettyBody,
                                                          attributes = Options}
                                                      }
        end,
  map_parse_func(Fun, FnName, Body, gen_statem).


map_parse_func(Fun, State, Body, Type) ->
  case eval_return(State, parse_body(Body), Type, []) of
    {error, _Reason} -> {error, bad_transition};
    List -> lists:map(Fun, List)
  end.

parse_body(Body) -> parse_body(Body, []).

parse_body([Statement|Rest], Acc) ->
  parse_body(Rest, Acc ++ [parse_statement(Statement)]);
parse_body([], Acc) -> lists:flatten([Acc]).
parse_statement({tuple, Line, Elems}) -> {tuple, Line, Elems};
parse_statement(Statement) when is_tuple(Statement) ->
  lists:map(fun(Index) ->  parse_statement(element(Index, Statement))
                              end, lists:seq(1, size(Statement)));
parse_statement(Statement) when is_list(Statement) ->
  lists:map(fun(Index) -> parse_statement(lists:nth(Index, Statement))
                            end, lists:seq(1, length(Statement)));
parse_statement(_Statement) -> [].

eval_return(State, [ReturnVal|Rest], gen_fsm, Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, init}|Acc]);
    {ok, {reply, _Reply, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {reply, _Reply, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {stop, _Reason, _Reply, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, sync}|Acc]);
    {ok, {next_state, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {stop, _Reason, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, async}|Acc]);
    _Other ->
      eval_return(State, Rest, gen_fsm, Acc)
  end;
eval_return(State, [ReturnVal|Rest], gen_statem, Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, init}|Acc]);
    {ok, ignore} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, init}|Acc]);
    {ok, stop} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop, _Reason, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop_and_reply, _Reason, _Replies}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop_and_reply, _Reason, _Replies, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {next_state, NextState, _Data}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, async}|Acc]);
    {ok, {keep_state, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, {keep_state, _NewData, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, keep_state_and_edge_data} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, {keep_state_and_edge_data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    _Other ->
      eval_return(State, Rest, gen_statem, Acc)
  end;
eval_return(_, [], _, []) ->
  {error, badreturn};
eval_return(_, [], _, Acc) ->
  Acc.


eval_tuple({tuple, Line, Elements})->
  ClearElements = lists:map(fun(Element) ->
                                case Element of
                                  {atom, RLine, Atom} -> {atom, RLine, Atom};
                                  _Other -> {atom, 0, '@var'}
                                end
                              end, Elements),
  ClearTuple = {tuple, Line, ClearElements},
  {value, Val, _} = erl_eval:expr(ClearTuple, []),
  {ok, Val};
eval_tuple(_Other)-> {error, not_a_tuple}.

expand_allstates(Edges, States) ->
  ClearStates = lists:delete(init, lists:delete(terminate, States)),
  lists:foldl(fun(Edge, Acc) ->
    #edge{from = FnName, to = To, edge_data = Data} = Edge,
    StateIndex = case FnName of
                   handle_event ->  1;
                   handle_sync_event -> 2;
                   handle_info -> 1
                 end,
    case To of
      '@var' ->
        case lists:nth(StateIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++ lists:map(fun(State) ->
              #edge{from = StateFrom, to = State, edge_data = Data}
            end, ClearStates);
          _ ->
            Acc ++ lists:map(fun(State) ->
              #edge{from = State, to = State, edge_data = Data}
            end, ClearStates)
        end;
      Other ->
        case lists:nth(StateIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++ [#edge{from = StateFrom, to = Other, edge_data = Data}];
          _ ->
            Acc ++ lists:map(fun(State) ->
              #edge{from = State, to = Other, edge_data = Data}
            end, ClearStates)
        end
    end
  end, [], Edges).

get_vertex(Graph, Label) ->
  Vertices = digraph:vertices(Graph),
  find_label(Label, Vertices, Graph).

find_label(Label, [V|Rest], Graph) ->
  case digraph:vertex(Graph, V) of
    {V, Label} -> V;
    _ -> find_label(Label, Rest, Graph)
  end;
find_label(_, [], _) -> {error, no_label}.

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].
