%% @doc Process a protocol structure into fsm edges and nodes
-module(generate).

-export([gen/2, gen_module/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

%% @doc represent endP as the special terminate function
end_state() ->
  Clause = ?Q(["(_Reason, _State, _Data) -> ok"]),
  {true, terminate, [Clause]}.


init_state(Id, [Edge], Nodes) ->
  NextState = list_to_atom("state" ++ integer_to_list(Edge#edge.to)),
  Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ 
                      atom_to_list(B) end, Edge#edge.edge_data#edge_data.comments),
  Cl = ?Q(["([]) -> {ok, '@NextState@', {} }"]),
  Clause = if
              length(Comms) > 0 -> erl_syntax:add_precomments(lists:map(fun(Com) -> 
                                            erl_syntax:comment([Com]) end, Comms),
                                            Cl);
              true -> Cl
           end,
  {true, init, [Clause]}.

%% @doc construct the clauses for the standard and choice states
clause(Event, Act, Var, Trans, NextState, Cons) ->
  Clause = ?Q(["('@Event@', {'@Act@', _@Var}, Data) ->",
        " {'@Trans@', '@NextState@', Data }"]),
  if
    length(Cons) > 0 -> erl_syntax:add_precomments(lists:map(fun(Com) -> 
                                   erl_syntax:comment([Com]) end, Cons), Clause);
    true -> Clause
  end.

%% @doc an extra clause for enter state
enter_clause() -> ?Q(["(enter, _OldState, _Data) -> keep_state_and_data"]).

%% @doc generates standard states, i.e. act x
std_state(Id, [Edge], Nodes) ->
  case maps:get(Edge#edge.to, Nodes) of
    end_state -> NextState = normal,
                 Trans = stop;
    _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#edge.to)),
             Trans = next_state
  end,
  {Act, Var} = Edge#edge.edge_data#edge_data.event,
  Event = Edge#edge.edge_data#edge_data.event_type,
  Cons = Edge#edge.edge_data#edge_data.comments,
  
  Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ 
                      atom_to_list(B) end, Cons),
  Clause = clause(Event, Act, merl:var(Var), Trans, NextState, Comms),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, [enter_clause(), Clause]}.

%% @doc generates choice states, i.e. branch
choice_state(Id, Edges, Nodes) ->
  Fun = fun(Edge) ->
    case maps:get(Edge#edge.to, Nodes) of
      end_state -> NextState = normal,
                   Trans = stop;
      _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#edge.to)),
               Trans = next_state
    end,
    {Act, Var} = Edge#edge.edge_data#edge_data.event,
    Event = Edge#edge.edge_data#edge_data.event_type,
    Cons = Edge#edge.edge_data#edge_data.comments,
    Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ atom_to_list(B) end, Cons),
    clause(Event, Act, merl:var(Var), Trans, NextState, Comms)
    end,

  Clauses = [enter_clause()] ++ lists:map(Fun, Edges),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, Clauses}.

%% @doc calls the appropriate function for choice and standard states
state_funs(K, V, Edges, Nodes) ->
  Pred = fun(Edge) -> Edge#edge.from =:= K end,
  Es = lists:filter(Pred, Edges),
  case V of
    init_state -> init_state(K, Es, Nodes);
    end_state -> end_state();
    choice_state -> choice_state(K, Es, Nodes);
    standard_state -> std_state(K, Es, Nodes)
  end.

%% @doc generate the callback functions
cb_fun(Data, NameMacro) ->
  {Act, Var} = Data#edge_data.event,
  Event = Data#edge_data.event_type,
  Var1 = merl:var(Var),
  Clauses = ?Q(["(_@Var1) ->",
             " gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
  {true, Act, [Clauses]}.


gen_module(FileName, P) ->
  Server = merl:var(list_to_atom("?SERVER")),
  Module = merl:var(list_to_atom("?MODULE")),
  Start = ?Q(["() -> ",
           "gen_statem:start_link({local, _@Server}, _@Module, [], []) "]),
  Cb = merl:quote(["() -> ",
           "[state_functions, state_enter]" ]),
  Stop = merl:quote(["() -> ",
            "gen_statem:stop(_@Server)"]),
  % Init = merl:quote(["([]) ->
  %              {ok, state1, {}}
  %           "]),

  {Edges, Nodes} = build_fsm:to_fsm(P),
  StateFuns = maps:fold(fun(K, V, AccIn) ->
                AccIn ++ [state_funs(K, V, Edges, Nodes)] end, [], Nodes),
  CBFuns = lists:foldl(fun(Edge,AccIn) ->
            AccIn ++ [cb_fun(Edge#edge.edge_data, Server)] end, [], lists:delete(hd(Edges), Edges)),
  Fs = [{true, start_link, [Start]},
        {true, callback_mode, [Cb]}
        % {true, init, [Init]}
        | StateFuns ] ++ lists:usort(CBFuns) ++ [{true, stop, [Stop]}],
  Forms = merl_build:add_attribute(behaviour, [merl:term('gen_statem')],
            merl_build:init_module(FileName)),
  Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),
  merl_build:module_forms(
          lists:foldl(fun ({X, Name, Cs}, S) ->
                              merl_build:add_function(X, Name, Cs, S)
                      end,
                      Forms1,
                      Fs)).

-spec gen(interleave:protocol(), string()) -> none().
gen(P, FileName) ->
    ModuleName = list_to_atom(lists:last(lists:droplast(string:tokens(FileName, "/.")))),
    Forms = gen_module(ModuleName, P),
    file:write_file(FileName,
                    erl_prettypr:format(erl_syntax:form_list(Forms),
                                        [{paper,160},{ribbon,80}])).
