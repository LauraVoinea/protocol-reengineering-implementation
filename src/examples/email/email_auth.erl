-module(email_auth).

-export([ step_one/0, step_two/0, security_question/0, email/0]).

%% two step email authentication

%% step one email address and password
step_one() ->
  {rec, "fst", {act, email, {branch, [{ok, {rec, "pass", {act, password, {branch, [{ok, {assert, one, endP}},
                                                                 {fail, {rvar, "pass"}}
                                                                ]}}}},
                         {fail, {rvar, "fst"}}
                                      ]}}}.

%% old step two security question
security_question() ->
  {rec, "sq", {require, one, {act, prompt, {act, reply, {branch, [{correct, endP},
                                                                 {incorrect, {rvar, "sq"}}]}}}}}.

%% current step two, app prompt or verification code
step_two() -> {rec, "scnd", {require, one, {branch, [{app, {act, prompt, {act, reply, {assert, auth, endP}}}},
                                            {code, {act, code, {act, reply, {assert, auth, endP}}}}
                                            ]}}}.

%% email menu
email() -> {rec, "menu", {require, auth, {branch, [{read, {act, read, {rvar, "menu"}}},
                                                   {send, {act, send, {rvar, "menu"}}},
                                                   {exit, {consume, auth, endP}}]}}}.
