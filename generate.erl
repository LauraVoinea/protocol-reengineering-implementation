-module(generate).

-compile(export_all).

-record(fsm, {name, actions, nextState}).

tofsm(P) -> tofsm(P, orddict:new(), orddict:new(), 0).

tofsm({act, Act, P}, S, RecMap, Id) ->
    Fsm = #fsm{name = "state" ++ integer_to_list(Id),
               actions = "act " ++ atom_to_list(Act),
               nextState = Id + 1},
    S1 = orddict:store(Id, Fsm, S),
    tofsm(P, S1, RecMap, Id + 1);
tofsm({branch, Branches}, S, RecMap, Id) ->
    {L, O, RecMap, Id} = lists:foldl(fun ({A, P}, Labels) ->
                                  Labels ++ " " ++ atom_to_list(A)
                                  % tofsm(P, S, RecMap, Id)
                          end,
                          "",
                          Branches),
    Fsm = #fsm{name = "choice" ++ integer_to_list(Id),
               actions = "branch " ++ L , nextState = Id + 1},
    S1 = orddict:store(Id, Fsm, S),
    tofsmBranches(Branches, S1, RecMap, Id + 1);
tofsm({rec, BoundVar, P}, S, RecMap, Id) ->
    RecMap1 = orddict:store(BoundVar, Id, RecMap),
    tofsm(P, S, RecMap1, Id);
tofsm({rvar, Var}, S, RecMap, Id) ->
    {ok, NextState} = orddict:find(Var, RecMap),
    O = orddict:update(Id - 1,
                       fun (Old) ->
                               Old#fsm{name = Old#fsm.name,
                                       actions = Old#fsm.actions,
                                       nextState = NextState}
                       end,
                       orddict:fetch(Id - 1, S),
                       S),
    {O, RecMap, Id};
tofsm(endP, S, RecMap, Id) ->
    {orddict:store(Id,
                   #fsm{name = "endP", actions = "end", nextState = Id},
                   S),
     RecMap,
     Id};
tofsm({_, _, P}, S, RecMap, Id) ->
    tofsm(P, S, RecMap, Id).

tofsmBranch({Label, P}, S, RecMap, Id) ->
    % S1 = orddict:store(Id, #fsm{name = Label}, S),
    tofsm(P, S, RecMap, Id).

tofsmBranches([], S, RecMap, Id) -> {S, RecMap, Id};
tofsmBranches([P], S, RecMap, Id) ->
    tofsmBranch(P, S, RecMap, Id);
tofsmBranches([P | PS], S, RecMap, Id) ->
    {S1, RecMap1, Id1} = tofsmBranch(P, S, RecMap, Id),
    tofsmBranches(PS, S1, RecMap1, Id1).

gen(File, P) ->
    {ok, IODevice} = file:open(File, [write]),
    LineSep = io_lib:nl(),
    [Name, _] = string:tokens(File, "."),
    io:format(IODevice, "-module(~s). ~s", [Name, LineSep]),
    io:format(IODevice,
              "-behaviour(gen_statem). ~s",
              [LineSep]),
    io:format(IODevice,
              "-define(NAME, ~s). ~s",
              [Name, LineSep]),

    io:format(IODevice,
              "~s ~n",
              ["-export([start_link/1,stop/0]).\n"]),
    io:format(IODevice, "~s ~n", ["-export([init/1]).\n"]),
    % "branch " ++
    %           lists:foldl(fun({A, _}, Labels) ->
    %              Labels ++ " " ++ atom_to_list(A) end, "", Branches),
    {Od, _, _} = tofsm(P),
    D = orddict:to_list(Od),
    % {K, V} = hd(D),
    io:format(IODevice, "~s ~n", [pprintStart()]),
    lists:foreach(fun ({_, V}) ->
                          io:format(IODevice, "~s ~n", [pprintState(V)])
                  end,
                  D),
    io:format(IODevice, "~s ~n", [pprintStop()]).

pprintStart() ->
    "start_link([]) -> \n \t gen_statem:start_link"
    "({local,?NAME}, ?MODULE, [], []). \n\n"
        ++
        "init([]) -> \n\t {ok, state0, []}. \n\n" ++
            "callback_mode() -> \n \t state_functions. "
            "\n\n".

pprintStop() ->
    "stop() -> \n \t gen_statem:stop(?NAME).\n".

pprintState(V) ->
    "% " ++
        V#fsm.actions ++
            "\n" ++
                V#fsm.name ++
                    "() -> \n" ++
                        "%.. code for actions here ... \n" ++
                            "\t {next_state, state" ++
                                integer_to_list(V#fsm.nextState) ++ ", {}}. \n".
