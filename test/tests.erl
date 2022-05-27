-module(tests).
-include_lib("eunit/include/eunit.hrl").

reeng_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        % Gen Fsm
        {"Create statem", fun reeng_generate_statem/0},
        % {"Extract protocol", fun reeng_extract/0},
        {"Compose strong", fun reeng_compose_strong/0},
        {"Compose weak", fun reeng_compose_weak/0},
        {"Compose correlating", fun reeng_compose_correlating/0},
        {"Compose all", fun reeng_compose_all/0}
          ]
    }.
reeng_compose_strong() ->
    ?assert(0 == length(reengineering:compose(examples:login(), examples:service(), strong))),
    ?assert(0 == length(reengineering:compose(examples:services(), examples:payments(), strong))),
    ?assert(1 == length(reengineering:compose(examples:payment(), examples:dispatch(), strong))),
    ?assert(0 == length(reengineering:compose(examples:http(), examples:aws_auth(), strong))),
    ?assert(0 == length(reengineering:compose(examples:login(), examples:booking(), strong))),
    ?assert(0 == length(reengineering:compose(examples:pin(), examples:tan(), strong))),
    ?assert(0 == length(reengineering:compose(examples:pintan(), examples:bank(), strong))),
    ?assert(1 == length(reengineering:compose(examples:resource(), examples:server(), strong))),
    ?assert(0 == length(reengineering:compose(examples:userAgent(), examples:agentInstrument(), strong))),
    ?assert(0 == length(reengineering:compose(examples:bankauthsimple(), examples:keycard(), strong)))
    .

reeng_compose_weak() ->
    ?assert(1 == length(reengineering:compose(examples:login(), examples:service(), weak))),
    ?assert(1 == length(reengineering:compose(examples:services(), examples:payments(), weak))),
    ?assert(1 == length(reengineering:compose(examples:payment(), examples:dispatch(), weak))),
    % ?assert(6 == length(reengineering:compose(examples:http(), examples:aws_auth(), weak))),
    ?assert(1 == length(reengineering:compose(examples:login(), examples:booking(), weak))),
    ?assert(1 == length(reengineering:compose(examples:pin(), examples:tan(), weak))),
    ?assert(2 == length(reengineering:compose(examples:pintan(), examples:bank(), weak))),
    ?assert(1 == length(reengineering:compose(examples:resource(), examples:server(), weak))),
    ?assert(0 == length(reengineering:compose(examples:userAgent(), examples:agentInstrument(), weak))),
    ?assert(2 == length(reengineering:compose(examples:bankauthsimple(), examples:keycard(), weak)))
    .

reeng_compose_correlating() ->
    ?assert(0 == length(reengineering:compose(examples:login(), examples:service(), correlating))),
    ?assert(1 == length(reengineering:compose(examples:services(), examples:payments(), correlating))),
    ?assert(1 == length(reengineering:compose(examples:payment(), examples:dispatch(), correlating))),
    % ?assert(0 == length(reengineering:compose(examples:http(), examples:aws_auth(), correlating))),
    ?assert(0 == length(reengineering:compose(examples:login(), examples:booking(), correlating))),
    ?assert(0 == length(reengineering:compose(examples:pin(), examples:tan(), correlating))),
    ?assert(0 == length(reengineering:compose(examples:pintan(), examples:bank(), correlating))),
    ?assert(1 == length(reengineering:compose(examples:resource(), examples:server(), correlating))),
    ?assert(2 == length(reengineering:compose(examples:userAgent(), examples:agentInstrument(), correlating))),
    ?assert(0 == length(reengineering:compose(examples:bankauthsimple(), examples:keycard(), correlating)))
    .

reeng_compose_all() ->
    ?assert(1 == length(reengineering:compose(examples:login(), examples:service(), all))),
    ?assert(2 == length(reengineering:compose(examples:services(), examples:payments(), all))),
    ?assert(1 == length(reengineering:compose(examples:payment(), examples:dispatch(), all))),
    % ?assert(9 == length(reengineering:compose(examples:http(), examples:aws_auth(), all))),
    ?assert(1 == length(reengineering:compose(examples:login(), examples:booking(), all))),
    ?assert(1 == length(reengineering:compose(examples:pin(), examples:tan(), all))),
    ?assert(2 == length(reengineering:compose(examples:pintan(), examples:bank(), all))),
    ?assert(2 == length(reengineering:compose(examples:resource(), examples:server(), all))),
    ?assert(2 == length(reengineering:compose(examples:userAgent(), examples:agentInstrument(), all))),
    ?assert(2 == length(reengineering:compose(examples:bankauthsimple(), examples:keycard(), all)))
    .


reeng_generate_statem() ->
    ?assert(ok == reengineering:generate(examples:http(), "src/examples/http/http_stub.erl")),
    ?assert(ok == reengineering:generate(examples:aws_auth(), "src/examples/http/aws_auth_stub.erl")),
    ?assert(ok == reengineering:generate(examples:login(), "src/examples/service/login_stub.erl")),
    ?assert(ok == reengineering:generate(examples:service(), "src/examples/service/service_stub.erl")),
    ?assert(ok == reengineering:generate(examples:services(), "src/examples/payment/services_stub.erl")),
    ?assert(ok == reengineering:generate(examples:payments(), "src/examples/payment/payments_stub.erl")),
    ?assert(ok == reengineering:generate(examples:payment(), "src/examples/dispatch/payment_stub.erl")),
    ?assert(ok == reengineering:generate(examples:dispatch(), "src/examples/dispatch/dispatch_stub.erl")),
    ?assert(ok == reengineering:generate(examples:booking(), "src/examples/booking/booking_stub.erl")),
    ?assert(ok == reengineering:generate(examples:resource(), "src/examples/resource/resource_stub.erl")),
    ?assert(ok == reengineering:generate(examples:server(), "src/examples/resource/server_stub.erl")),
    ?assert(ok == reengineering:generate(examples:pin(), "src/examples/pintan/pin_stub.erl")),
    ?assert(ok == reengineering:generate(examples:tan(), "src/examples/pintan/tan_stub.erl")),
    ?assert(ok == reengineering:generate(examples:bank(), "src/examples/banking/bank_stub.erl")),
    ?assert(ok == reengineering:generate(examples:pintan(), "src/examples/banking/pintan_stub.erl")),
    ?assert(ok == reengineering:generate(examples:agent1(), "src/examples/agents/agent1_stub.erl")),
    ?assert(ok == reengineering:generate(examples:agent2(), "src/examples/agents/agent2_stub.erl"))
    .

reeng_extract() ->
    ?assert(equals(examples:http(), reengineering:extract("src/examples/http/http_stub.erl"))),
    ?assert(equals(examples:aws_auth(), reengineering:extract("src/examples/http/aws_auth_stub.erl"))),
    ?assert(equals(examples:login(), reengineering:extract("src/examples/service/login_stub.erl"))),
    ?assert(equals(examples:service(), reengineering:extract("src/examples/service/service_stub.erl"))),
    ?assert(equals(examples:services(), reengineering:extract("src/examples/payment/services_stub.erl"))),
    ?assert(equals(examples:payments(), reengineering:extract("src/examples/payment/payments_stub.erl"))),
    ?assert(equals(examples:payment(), reengineering:extract("src/examples/dispatch/payment_stub.erl"))),
    ?assert(equals(examples:dispatch(), reengineering:extract("src/examples/dispatch/dispatch_stub.erl"))),
    ?assert(equals(examples:booking(), reengineering:extract("src/examples/booking/booking_stub.erl"))),
    ?assert(equals(examples:resource(), reengineering:extract("src/examples/resource/resource_stub.erl"))),
    ?assert(equals(examples:server(), reengineering:extract("src/examples/resource/server_stub.erl"))),
    ?assert(equals(examples:pin(), reengineering:extract("src/examples/pintan/pin_stub.erl"))),
    ?assert(equals(examples:tan(), reengineering:extract("src/examples/pintan/tan_stub.erl"))),
    ?assert(equals(examples:bank(), reengineering:extract("src/examples/banking/bank_stub.erl"))),
    ?assert(equals(examples:agent1(), reengineering:extract("src/examples/agents/agent1_stub.erl"))),
    ?assert(equals(examples:agent2(), reengineering:extract("src/examples/agents/agent2_stub.erl")))
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
