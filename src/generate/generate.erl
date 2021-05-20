%% @doc Process a protocol structure into fsm edges and nodes
-module(generate).

-export([gen/2, gen_module/2]).

-include_lib("syntax_tools/include/merl.hrl").
-include("reng.hrl").

%% @doc represent endP as the special terminate function
end_state() ->
  Clause = ?Q(["(_Reason, _State, _Data) -> ok"]),
  {true, terminate, [Clause], [""]}.

%% @doc construct the clauses for the standard and choice states
clause(Event, Act, Var, Trans, NextState, _Cons) ->
  ?Q(["('@Event@', {'@Act@', _@Var}, Data) ->",
         " {'@Trans@', '@NextState@', Data }"]).
% clause(Event, Act, V, Trans, NextState, Cons) ->
%   Fun = fun({require, Var}, Body) ->
%           FnName = list_to_atom(atom_to_list(require) ++ "_" ++ atom_to_list(Var)),
%           {?Q(["case '@FnName@'(Data) of",
%                       "true -> _@Body;",
%                       "false -> {keep_state_and_data}",
%                       "end"]),
%           ?Q(["case '@FnName@' (Data) of",
%                       "true -> _@Body;",
%                       "false -> {keep_state_and_data}",
%                       "end"])};
%             ({assert, Var}, Body) ->
%               FnName = list_to_atom(atom_to_list(assert) ++ "_" ++ atom_to_list(Var)),
%              {?Q(["case '@FnName@' (Data) of",
%                         "{true, NewData} -> _@Body;",
%                         "false -> {keep_state_and_data}",
%                         "end"]),
%             ?Q(["case '@FnName@' (Data) of",
%                        "{true, NewData} -> _@Body;",
%                        "false -> {keep_state_and_data}",
%                        "end"])};
%             ({consume, Var}, Body) ->
%               FnName = list_to_atom(atom_to_list(consume) ++ "_" ++ atom_to_list(Var)),
%              {?Q(["case consume_@Var(Data) of",
%                         "{true, NewData} -> _@Body;",
%                         "false -> {keep_state_and_data}",
%                         "end"]),
%             ?Q(["case '@FnName@' (Data) of",
%                        "{true, NewData} -> _@Body;",
%                        "false -> {keep_state_and_data}",
%                        "end"])}
%           end,

%   C = ?Q(["{'@Trans@', '@NextState@', Data }"]),
%   {_, Acc1} = lists:mapfoldl(Fun, C,  lists:reverse(Cons)),
%   ?Q(["('@Event@', {'@Act@', _@V}, Data) -> _@Acc1 "]).

%% @doc an extra clause for enter state
enter_clause() -> ?Q(["(enter, _OldState, _Data) -> keep_state_and_data"]).

%% @doc generates standard states, i.e. act x
std_state(Id, [Edge], Nodes) ->
  case maps:get(Edge#trans.to, Nodes) of
    end_state -> NextState = normal,
                 Trans = stop;
    _Else -> NextState = list_to_atom("state" ++ integer_to_list(Edge#trans.to)),
             Trans = next_state
  end,
  Act = Edge#trans.data#data.action,
  Var = merl:var(Edge#trans.data#data.var),
  Event = Edge#trans.data#data.event,
  Cons = Edge#trans.data#data.cons,
  Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ 
                      atom_to_list(B) end, Cons),
  Clause = clause(Event, Act, Var, Trans, NextState, Cons),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, [enter_clause(), Clause], [Comms]}.

%% @doc generates choice states, i.e. branch
choice_state(Id, Edges, Nodes) ->
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
    Cons = Edge#trans.data#data.cons,
    clause(Event, Act, Var, Trans, NextState, Cons)
    end,
  Cons = lists:foldl(fun(Edge, Con) -> 
    Con ++ Edge#trans.data#data.cons end, [], Edges),
  Comms = lists:map(fun({A, B}) ->  atom_to_list(A) ++ " " ++ 
                    atom_to_list(B) end, Cons),
  Clauses = [enter_clause()] ++ lists:map(Fun, Edges),
  Name = list_to_atom("state" ++ integer_to_list(Id)),
  {true, Name, Clauses, [Comms]}.

%% @doc calls the appropriate function for choice and standard states
state_funs(K, V, Edges, Nodes) ->
  case V of
    end_state -> end_state();
    choice_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Branches = lists:filter(Pred, Edges),
      choice_state(K, Branches, Nodes);
    standard_state ->
      Pred = fun(Edge) -> Edge#trans.from =:= K end,
      Edge = lists:filter(Pred, Edges),
      std_state(K, Edge, Nodes)
  end.

%% @doc generate the callback functions
cb_fun(#data{action = Act, var = Var, event = Event}, NameMacro) ->
  Var1 = merl:var(Var),
  Clauses = ?Q(["(_@Var1) ->",
             " gen_statem:'@Event@'(_@NameMacro, {'@Act@',  _@Var1})"
            ]),
  {true, Act, [Clauses], [""]}.

%% @doc generate constraint functions
cons_funs(#data{cons = Cons}) ->
  lists:map(fun({Con, Var}) ->
            Name = list_to_atom(atom_to_list(Con) ++ "_" ++ atom_to_list(Var)),
            Clauses = case Con of
                            require -> ?Q(["(Data) -> true"]);
                            assert ->  ?Q(["(Data) -> {true, Data}"]);
                            consume ->  ?Q(["(Data) -> {true, Data}"])
                      end,
            {true, Name, [Clauses],[""]}
          end, Cons).

gen_module(FileName, P) ->
  Server = merl:var(list_to_atom("?SERVER")),
  Module = merl:var(list_to_atom("?MODULE")),
  Start = ?Q(["() -> ",
           "gen_statem:start_link({local, _@Server}, _@Module, [], []) "]),
  Cb = merl:quote(["() -> ",
           "[state_functions, state_enter]" ]),
  Stop = merl:quote(["() -> ",
            "gen_statem:stop(_@Server)"]),
  Init = merl:quote(["([]) ->
               {ok, state1, {}}
            "]),

  {Edges, Nodes} = build_fsm:to_fsm(P),
  StateFuns = maps:fold(fun(K, V, AccIn) ->
                AccIn ++ [state_funs(K, V, Edges, Nodes)] end, [], Nodes),
  CBFuns = lists:foldl(fun(Edge,AccIn) ->
            AccIn ++ [cb_fun(Edge#trans.data, Server)] end, [], Edges),
  ConsFuns = lists:foldl(fun(Edge,AccIn) ->
          AccIn ++ cons_funs(Edge#trans.data) end, [], Edges),
  Fs = [{true, start_link, [Start], ""},
        {true, callback_mode, [Cb], ""},
        {true, init, [Init], ""}
        | StateFuns ] ++ lists:usort(CBFuns) ++ [{true, stop, [Stop], ""}],
        %  ++ lists:usort(ConsFuns),
  Forms = merl_build:add_attribute(behaviour, [merl:term('gen_statem')],
            merl_build:init_module(FileName)),
  Forms1 = merl_build:add_attribute(define, [merl:var('SERVER'), Module], Forms),
  merl_build:module_forms(
          lists:foldl(fun ({X, Name, Cs, Comms}, S) ->
                              merl_build:add_function(X, Name, Cs, Comms, S)
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
