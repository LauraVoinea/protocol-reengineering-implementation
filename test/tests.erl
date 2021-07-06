-module(tests).
-include_lib("eunit/include/eunit.hrl").

reeng_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        % Gen Fsm
        {"Create statem", fun reeng_generate_statem/0},
        {"Extract protocol", fun reeng_extract/0}

          ]
    }.

reeng_generate_statem() ->
    ?assert(ok == reengineering:generate(examples:e1(), "e1.erl")),
    ?assert(ok == reengineering:generate(examples:e2(), "e2.erl")),
    ?assert(ok == reengineering:generate(examples:e3(), "e3.erl")),
    ?assert(ok == reengineering:generate(examples:e4(), "e4.erl")),
    ?assert(ok == reengineering:generate(examples:e5(), "e5.erl")),
    ?assert(ok == reengineering:generate(examples:e6(), "e6.erl")),
    ?assert(ok == reengineering:generate(examples:e7(), "e7.erl")),
    ?assert(ok == reengineering:generate(examples:e8(), "e8.erl")),
    ?assert(ok == reengineering:generate(examples:e9(), "e9.erl")),
    ?assert(ok == reengineering:generate(examples:e10(), "e10.erl")),
    ?assert(ok == reengineering:generate(examples:bank(), "bank.erl")),
    ?assert(ok == reengineering:generate(examples:pintan(), "pintan.erl")),
    ?assert(ok == reengineering:generate(examples:agent1(), "agent1.erl")),
    ?assert(ok == reengineering:generate(examples:agent2(), "agent2.erl"))
    .

reeng_extract() ->
    ?assert(equals(examples:e1(), reengineering:extract("e1.erl"))),
    ?assert(equals(examples:e2(), reengineering:extract("e2.erl"))),
    ?assert(equals(examples:e3(), reengineering:extract("e3.erl"))),
    ?assert(equals(examples:e4(), reengineering:extract("e4.erl"))),
    ?assert(equals(examples:e5(), reengineering:extract("e5.erl"))),
    ?assert(equals(examples:e6(), reengineering:extract("e6.erl"))),
    ?assert(equals(examples:e7(), reengineering:extract("e7.erl"))),
    ?assert(equals(examples:e8(), reengineering:extract("e8.erl"))),
    ?assert(equals(examples:e9(), reengineering:extract("e9.erl"))),
    ?assert(equals(examples:e10(), reengineering:extract("e10.erl"))),
    ?assert(equals(examples:bank(), reengineering:extract("bank.erl"))),
    ?assert(equals(examples:pintan(), reengineering:extract("pintan.erl"))),
    ?assert(equals(examples:agent1(), reengineering:extract("agent1.erl"))),
    ?assert(equals(examples:agent2(), reengineering:extract("agent2.erl")))
    .

equals_br({Label1, P1}, {Label2, P2}) -> 
    Var1 = lists:last(string:tokens(atom_to_list(Label1), "_")),
    Var2 = lists:last(string:tokens(atom_to_list(Label2), "_")),
    string:equal(Var1, Var2) and equals(P1, P2);
equals_br(_, _) -> false.

equals_brs([], []) -> true;
equals_brs([Br1], [Br2]) -> equals_br(Br1, Br2);
equals_brs([Br1|Brs1], [Br2|Brs2]) -> equals_br(Br1, Br2) and equals_brs(Brs1, Brs2).

equals({act, Act1, P1}, {act, Act2, P2}) ->
    Var1 = lists:last(string:tokens(atom_to_list(Act1), "_")),
    Var2 = lists:last(string:tokens(atom_to_list(Act2), "_")),
    string:equal(Var1, Var2) and equals(P1, P2);
equals({branch, Branches1}, {branch, Branches2}) ->
    equals_brs(lists:usort(Branches1), lists:usort(Branches2));
equals({assert, N, P1}, {assert, N, P2}) -> equals(P1, P2);
equals({require, N, P1}, {require, N, P2}) ->
    equals(P1, P2);
equals({consume, N, P1}, {consume, N, P2}) ->
    equals(P1, P2);
equals({rec, _, P1}, {rec, _, P2}) ->
  equals(P1, P2);
equals({rvar, _}, {rvar, _}) ->
    true;
equals(endP, endP) ->
    true;
equals(_, _) -> 
    false.


setup() ->
    application:start(protocol_reengineering),
    stuff.

cleanup(stuff) ->
  application:stop(protocol_reengineering),
  ok.
