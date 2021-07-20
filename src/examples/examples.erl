-module(examples).

-export([e1/0, e2/0, e3/0, e4/0, e5/0, e6/0, e7/0, e8/0, e9/0, e10/0,
          bank/0, pintan/0, pin/0, tan/0, bank_pt/0, agent1/0, agent2/0, interleavings/0]).


% # Examples
e1() ->
  {act, n, endP}.

e2() ->
  {require, n, {act, x, endP}}.

e3() ->
  {assert, n, {act, y, endP}}.

e4() ->
  {branch, [{l, {act, b, {assert, n, endP}}} ,{r, {act, c, {assert, n, endP}}}]}.

e5() ->
  {branch, [{l, {assert, n, endP}} ,{r, {assert, n, endP}}, {m, {assert, n, endP}}]}.

e6() ->
  {branch, [{l, {require, n, endP}} ,{r, {act, c, endP}}, {m, {assert, n, endP}}]}.

e7() ->
  {act, r_pwd, {branch, [{ok, {assert, n, endP}},{fail, endP}]}}.

e8() ->
  {require, n, {act, do_banking, endP}}.

e9() ->
  {rec, "x", {act, a, {act, b, {rvar, "x"}}}}.

e10() ->
  {rec, "y", {act, a, {branch, [{l, {act, b, {require, n, endP}}}
                               ,{r, {rvar, "y"}}]}}}.

bank() ->
  {require, pin, {rec, t, {branch, [{statement, {act, s_statement, {rvar, t}}},
                                    {payment, {assert, pay,{consume, tan,{act, r_details,  {rvar, t}}}}},
                                    {logout, {consume, pin, endP}}]
                          }
                  }
  }.

pintan() ->
  {act, r_pin, {branch, [{ok, {assert, pin, {rec, r, {consume, pay, ctan()}}}},
                         {fail, endP}]
                }
  }.

ctan() ->
   {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, r}}},
                                            {fail, {rvar, r}}]
                            }
              }
  }.


bankauthsimple() ->
{rec,t,
          {branch,
              [ {payment, {assert, keyp,  {require, tb, {act,s_id, {act,r_tan, {branch,
                                      [  {tok,{assert,tan,{consume,tan,{act,r_details,{rvar,t}}}}},
                                          {tfail,{rvar,t}}
                                      ]}}
                          }}
                }},
                {statement,{act,s_statement,{rvar,t}}},
                 {logout,endP}
               ]
            }
}.


keycard() -> {rec, y, {require, keyp, {branch, [{tan, {assert, tb, {rvar, y}}},
                         {keycard, {rvar, y}}
                                  ]
                        }}
          }.


pin() ->
  {act, r_pin, {branch, [{ok, {assert, pin, endP}},
                                {fail, endP}]
                }
  }.

tan() ->
  {require, pin, {rec, r, {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, r}}},
                                                              {fail, {rvar, r}}]
                                                    }
                                      }
                          }
                  }
  }.

agent1() -> {branch, [{r_ua_set_ua_set, {assert, n, {assert, set, {act, r_ua_coord, {assert, coord, {act, s_au_state, endP}}}}}},
                               {r_ua_get, {assert, n, {assert, get,{act, s_au_snap, {assert, snap, endP}}}}},
                               {r_ua_close,{assert, n, {assert, close, endP}}}]
            }.

agent2() -> {consume, n, {branch, [{s_ai_set, {consume, set, {act, s_ai_coord, {consume, coord, {act, r_ia_state, endP}}}}},
                               {s_ai_get, {consume, get, {act, r_ia_snap, {consume, snap, endP}}}},
                               {s_ai_close, {consume, close, endP}}]
            }}.
            
            

userAgent() -> {rec, r, {branch, [  {ua_r_set, {act, ua_r_coord, {assert, set, {rvar, r}}}},
                                    {ua_r_get, {assert, get, {consume, snap, {act, au_s_snap, {rvar, r}}}}},
                                    {ua_r_close, {assert, close, endP}}
]}}.

agentInstrument() -> {rec, t, {branch, [  {ai_s_set, {consume, set, {act, ai_s_coord, {rvar, t}}}},
                                          {ai_s_get, {consume, get, {act, ai_r_snap, {assert, snap, {rvar, t}}}}},
                                          {ui_s_close, {consume, close, endP}}
]}}.


bank_pt() -> {act,r_pin,
{branch,
 [{ok,
   {assert,pin,
    {require,pin,
     {rec,t,
        {branch,
         [{payment, 
         {act,s_id,
         {act,r_tan,
           {branch,
            [{ok,{assert,tan,{consume,tan,{act,r_details,{rvar,t}}}}},
             {fail,{rvar,t}}]}}}},
          {statement,{act, s_statement,{rvar,t}}},
          {logout,{consume,pin,endP}}]}}}}},
  {fail,endP}]}}.
  

%% compute the number of interleavings for the different composition options
interleavings() ->
  Protocols = [{e7(), e2()}, {e2(), e3()}, {e2(), e4()}, {e2(), e5()}, 
               {e6(), e3()}, {e7(), e8()}, {e3(), e10()}, {pin(), tan()},
               {e5(), e10()}, {userAgent(), agentInstrument()}, {bankauthsimple(), keycard()}],
  Strong = lists:map(fun({P1, P2}) -> length(interleave:interleave(P1, P2)) end, Protocols),
  Weak = lists:map(fun({P1, P2}) -> length(interleave:interleaveWeak(P1, P2)) end, Protocols),
  Correlating = lists:map(fun({P1, P2}) -> length(interleave:interleaveCorrelating(P1, P2)) end, Protocols),
  WeakCorrelating = lists:map(fun({P1, P2}) -> length(interleave:interleaveAll(P1, P2)) end, Protocols), 
  io:format("Strong: ~p~n", [Strong]),
  io:format("Weak: ~p~n", [Weak]),
  io:format("Correlating: ~p~n", [Correlating]),
  io:format("Weak Correlating: ~p~n", [WeakCorrelating]).