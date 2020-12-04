-module(protocol_reengineering).

-export([]).


% # Examples
example1() ->
  {rec, "x", {act, a, {act, b, {rvar, "x"}}}}.

example2() ->
  {rec, "x", {act, a, {branch, [{l, {act, b, {require, n, endP}}}
                               ,{r, {rvar, "x"}}]}}}.

example3() ->
  {branch, [{l, {act, b, {assert, n, endP}}} ,{r, {act, c, {assert, n, endP}}}]}.

example4() ->
  {require, n, {act, x, endP}}.

e1() -> {branch, [{l, {assert, n, endP}} ,{r, {assert, n, endP}}, {m, {assert, n, endP}}]}.
e2() -> {act, n, endP}.
e3() -> {branch, [{l, {require, n, endP}} ,{r, {act, c, endP}}, {m, {assert, n, endP}}]}.



sa() -> {act, r_pwd, {branch, [{ok, {assert, n, endP}},{fail, endP}]}}.
sb() -> {require, n, {act, do_banking, endP}}.



bank() -> {require, pin, {rec, t, {branch, [{payment, {act, s_sdata, {consume, tan, {rvar, t}}}},
                                          {statement, {act, r_pdata, {rvar, t}}},
                                          {logout, endP}]
                                }
                        }
        }.

pintan() -> {act, r_pin, {branch, [{ok, {assert, pin, {rec, r, {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, r}}},
                                                                                              {fail, {rvar, r}}]
                                                                                    }

                                                                      }
                                                            }
                                                  }
                                    }
                                },
                              {fail, endP},
                              {logout, endP}]
                      }
        }.
