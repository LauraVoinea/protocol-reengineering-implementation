-module(email_auth).



%% two step email authentication

%% step one email and password
first_step() -> {act, email, {branch, [ {ok, rec, p, {act, password, {branch, [ {ok, {assert, password, endP}},
                                                                                {fail, {rvar, p}}
                                                                              ]}}},
                                        {fail, endP}
                                      ]}}.
%% old step two security question
security_question() -> {require, password, {act, prompt, {act, reply, {assert, auth, endP}}}}.
%% current step two, app prompt or verification code
step_two() -> {require, password, {branch, [{app, {act, prompt, {act, reply, {assert, auth, endP}}}},
                                            {code, {act, code, {act, reply, {assert, auth, endP}}}}
                                            ]}}.
login() -> {require, auth, {branch, [ {ok, endP},
                    {fail, endP}
                    ]}}.
