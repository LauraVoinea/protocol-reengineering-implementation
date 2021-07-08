-module(examples).

-export([e1/0, e2/0, e3/0, e4/0, e5/0, e6/0, e7/0, e8/0, e9/0, e10/0,
          bank/0, pintan/0, pin/0, tan/0, bank_pt/0, agent1/0, agent2/0]).



% # Examples
e1() ->
  {act, n, endP}.

e2() ->
  {assert, n,{require, n, {act, x, endP}}}.

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
  {require, pin, {rec, t, {branch, [{payment, {consume, tan,{act, r_payment,  {rvar, t}}}},
                                          {statement, {act, s_statement, {rvar, t}}},
                                          {logout, endP}]
                          }
                  }
  }.

pintan() ->
  {act, r_pin, {branch, [
                                    {ok, {assert, pin, {rec, r, ctan()}}},
                                    {fail, endP}]
                }
  }.

ctan() ->
  {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, r}}},
                                            {fail, {rvar, r}}]
                            }
              }
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

agent1() -> {branch, [{r_ua_set_ua_set, {assert, n, {assert, set, {act, r_ua_coord, {assert, coord, {act, s_au_state, endP}}}}}},
                               {r_ua_get, {assert, n, {assert, get,{act, s_au_snap, {assert, snap, endP}}}}},
                               {r_ua_close,{assert, n, {assert, close, endP}}}]
            }.

agent2() -> {consume, n, {branch, [{s_ai_set, {consume, set, {act, s_ai_coord, {consume, coord, {act, r_ia_state, endP}}}}},
                               {s_ai_get, {consume, get, {act, r_ia_snap, {consume, snap, endP}}}},
                               {s_ai_close, {consume, close, endP}}]
            }}.

we1() -> {branch, [{ra, {assert, n, {assert, a, endP}}}, {rb, {assert, n, {assert, b, endP}}}]}.
we2() -> {consume, n, {branch, [{sa, {consume, a, endP}}, {sb, {consume, b, endP}}]}}.

we3() -> {branch, [{ra, {assert, a, endP}}, {rb, {assert, b, endP}}]}.
we4() -> {branch, [{sa, {consume, a, endP}}, {sb, {consume, b, endP}}]}.

we5() -> {branch, [{ra, {assert, a, endP}}, {rb, {consume, b, endP}}]}.
we6() -> {branch, [{sa, {consume, a, endP}}, {sb, {assert, b, endP}}]}.
