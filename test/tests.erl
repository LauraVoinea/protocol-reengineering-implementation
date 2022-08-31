-module(tests).
-include_lib("eunit/include/eunit.hrl").

reeng_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        % Gen Fsm
        {"Create statem", fun reeng_generate_statem/0},
        {"Extract protocol", fun reeng_extract/0},
        {"Compose strong", fun reeng_compose_strong/0},
        {"Compose weak", fun reeng_compose_weak/0},
        {"Compose correlating", fun reeng_compose_correlating/0},
        {"Compose all", fun reeng_compose_all/0}
          ]
    }.
reeng_compose_strong() ->
    ?assert(0 == length(reeng:compose(examples:login(), examples:service(), strong))),
    ?assert(0 == length(reeng:compose(examples:s1(), examples:s2(), strong))),
    ?assert(1 == length(reeng:compose(examples:i1(), examples:i2(), strong))),
    ?assert(0 == length(reeng:compose(examples:http(), examples:aws_auth(), strong))),
    ?assert(0 == length(reeng:compose(examples:login(), examples:booking(), strong))),
    ?assert(0 == length(reeng:compose(examples:pin(), examples:tan(), strong))),
    ?assert(0 == length(reeng:compose(examples:pintan(), examples:bank(), strong))),
    ?assert(1 == length(reeng:compose(examples:resource(), examples:server(), strong))),
    ?assert(0 == length(reeng:compose(examples:userAgent(), examples:agentInstrument(), strong))),
    ?assert(0 == length(reeng:compose(examples:auth_two_step(), examples:email(), strong)))
    .

reeng_compose_weak() ->
    ?assert(1 == length(reeng:compose(examples:login(), examples:service(), weak))),
    ?assert(1 == length(reeng:compose(examples:s1(), examples:s2(), weak))),
    ?assert(1 == length(reeng:compose(examples:i1(), examples:i2(), weak))),
    ?assert(6 == length(reeng:compose(examples:http(), examples:aws_auth(), weak))),
    ?assert(1 == length(reeng:compose(examples:login(), examples:booking(), weak))),
    ?assert(1 == length(reeng:compose(examples:pin(), examples:tan(), weak))),
    ?assert(1 == length(reeng:compose(examples:pintan(), examples:bank(), weak))),
    ?assert(1 == length(reeng:compose(examples:resource(), examples:server(), weak))),
    ?assert(0 == length(reeng:compose(examples:userAgent(), examples:agentInstrument(), weak))),
    ?assert(9 == length(reeng:compose(examples:auth_two_step(), examples:email(), weak)))
    .

reeng_compose_correlating() ->
    ?assert(0 == length(reeng:compose(examples:login(), examples:service(), correlating))),
    ?assert(2 == length(reeng:compose(examples:s1(), examples:s2(), correlating))),
    ?assert(1 == length(reeng:compose(examples:i1(), examples:i2(), correlating))),
    % ?assert(0 == length(reeng:compose(examples:http(), examples:aws_auth(), correlating))),
    ?assert(0 == length(reeng:compose(examples:login(), examples:booking(), correlating))),
    ?assert(0 == length(reeng:compose(examples:pin(), examples:tan(), correlating))),
    ?assert(0 == length(reeng:compose(examples:pintan(), examples:bank(), correlating))),
    ?assert(1 == length(reeng:compose(examples:resource(), examples:server(), correlating))),
    ?assert(2 == length(reeng:compose(examples:userAgent(), examples:agentInstrument(), correlating))),
    ?assert(0 == length(reeng:compose(examples:auth_two_step(), examples:email(), correlating)))
    .

reeng_compose_all() ->
    ?assert(1 == length(reeng:compose(examples:login(), examples:service(), all))),
    ?assert(3 == length(reeng:compose(examples:s1(), examples:s2(), all))),
    ?assert(1 == length(reeng:compose(examples:i1(), examples:i2(), all))),
    % ?assert(6 == length(reeng:compose(examples:http(), examples:aws_auth(), all))),
    ?assert(1 == length(reeng:compose(examples:login(), examples:booking(), all))),
    ?assert(1 == length(reeng:compose(examples:pin(), examples:tan(), all))),
    ?assert(1 == length(reeng:compose(examples:pintan(), examples:bank(), all))),
    ?assert(2 == length(reeng:compose(examples:resource(), examples:server(), all))),
    ?assert(2 == length(reeng:compose(examples:userAgent(), examples:agentInstrument(), all))),
    ?assert(9 == length(reeng:compose(examples:auth_two_step(), examples:email(), all)))
    .


reeng_generate_statem() ->
    ?assert(ok == reeng:generate(examples:http(), "src/examples/http/http_stub.erl")),
    ?assert(ok == reeng:generate(examples:aws_auth(), "src/examples/http/aws_auth_stub.erl")),
    ?assert(ok == reeng:generate(examples:login(), "src/examples/service/login_stub.erl")),
    ?assert(ok == reeng:generate(examples:s1(), "src/examples/service/s1_stub.erl")),
    ?assert(ok == reeng:generate(examples:s2(), "src/examples/service/s2_stub.erl")),
    ?assert(ok == reeng:generate(examples:i1(), "src/examples/payment/i1_stub.erl")),
    ?assert(ok == reeng:generate(examples:i2(), "src/examples/payment/i2_stub.erl")),
    ?assert(ok == reeng:generate(examples:booking(), "src/examples/booking/booking_stub.erl")),
    ?assert(ok == reeng:generate(examples:resource(), "src/examples/resource/resource_stub.erl")),
    ?assert(ok == reeng:generate(examples:server(), "src/examples/resource/server_stub.erl")),
    ?assert(ok == reeng:generate(examples:pin(), "src/examples/pintan/pin_stub.erl")),
    ?assert(ok == reeng:generate(examples:tan(), "src/examples/pintan/tan_stub.erl")),
    ?assert(ok == reeng:generate(examples:bank(), "src/examples/banking/bank_stub.erl")),
    ?assert(ok == reeng:generate(examples:pintan(), "src/examples/banking/pintan_stub.erl")),
    ?assert(ok == reeng:generate(examples:agent1(), "src/examples/agents/agent1_stub.erl")),
    ?assert(ok == reeng:generate(examples:agent2(), "src/examples/agents/agent2_stub.erl"))
    .

reeng_extract() ->
    ?assert(equals(examples:http(), reeng:extract("src/examples/http/http_stub.erl"))),
    ?assert(equals(examples:aws_auth(), reeng:extract("src/examples/http/aws_auth_stub.erl"))),
    ?assert(equals(examples:login(), reeng:extract("src/examples/service/login_stub.erl"))),
    ?assert(equals(examples:s1(), reeng:extract("src/examples/service/s1_stub.erl"))),
    ?assert(equals(examples:s2(), reeng:extract("src/examples/service/s2_stub.erl"))),
    ?assert(equals(examples:i1(), reeng:extract("src/examples/payment/i1_stub.erl"))),
    ?assert(equals(examples:i2(), reeng:extract("src/examples/payment/i2_stub.erl"))),
    ?assert(equals(examples:booking(), reeng:extract("src/examples/booking/booking_stub.erl"))),
    ?assert(equals(examples:resource(), reeng:extract("src/examples/resource/resource_stub.erl"))),
    ?assert(equals(examples:server(), reeng:extract("src/examples/resource/server_stub.erl"))),
    ?assert(equals(examples:pin(), reeng:extract("src/examples/pintan/pin_stub.erl"))),
    ?assert(equals(examples:tan(), reeng:extract("src/examples/pintan/tan_stub.erl"))),
    ?assert(equals(examples:bank(), reeng:extract("src/examples/banking/bank_stub.erl"))),
    ?assert(equals(examples:agent1(), reeng:extract("src/examples/agents/agent1_stub.erl"))),
    ?assert(equals(examples:agent2(), reeng:extract("src/examples/agents/agent2_stub.erl")))
    .

equals_br({Label1, P1}, {Label2, P2}) ->
    Var1 = lists:last(string:tokens(atom_to_list(Label1), "_")),
    Var2 = lists:last(string:tokens(atom_to_list(Label2), "_")),
    string:equal(Var1, Var2, true) and equals(P1, P2);
equals_br(V1, V2) -> false.

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
equals(V1, V2) ->
    false.


setup() ->
    application:start(reengineering),
    stuff.

cleanup(stuff) ->
  application:stop(reengineering),
  ok.
