-module(examples).

-compile(export_all).
-compile(nowarn_export_all).


%%Examples
service() ->
  {require, n, {act, x, endP}}.

login() ->
  {act, r_pwd, {branch, [{ok, {assert, n, endP}},{fail, endP}]}}.

booking() ->
  {require, n, {act, do_booking, endP}}.


server() ->
  {rec, "y", {act, request, {branch, [{accept, {act, b, {require, n, endP}}}
                               ,{ignore, {rvar, "y"}}]}}}.
resource() ->
 {branch, [{l, {assert, n, endP}} ,{r, {assert, n, endP}}, {m, {assert, n, endP}}]}.
%%Running Examples

%% example from 2.1
% I1
payment() ->
  {act, pay, {assert, paid, endP}}.
% I2
dispatch() ->
  {consume, paid, {act, item, endP}}.

%% correlating branching example from 3.1.2
% S1
services() -> {branch, [{s1,{assert, one, endP}}, {s2, {assert, two, endP}}]}.
% S2
payments() -> {branch, [{p1,{consume, one, endP}}, {p2,{consume, two, endP}}]}.


%% Example 3 Fairness and weak branching
% SA
s_a() -> {branch, [{ok, {assert, n, endP}},
                  {ko, endP}]}.
% SB
s_b() -> {require, n, endP}.
% SAB
s_ab() -> {branch, [{ok, {assert, n, {require, n, endP}}},
                  {ko, endP}]}.

%% PINTAN example
bank() ->
  {require, pin, {rec, "t", {branch, [{statement, {act, s_statement, {rvar, "t"}}},
                                    {payment, {assert, pay,{consume, tan, {act, r_details,  {rvar, "t"}}}}},
                                    {logout, {consume, pin, endP}}]
                          }
                  }
  }.

pintan() ->
  {act, r_pin, {branch, [{ok, {assert, pin, {rec, "r", {consume, pay, ctan()}}}},
                         {fail, endP}]
                }
  }.

ctan() ->
   {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, "r"}}},
                                            {fail, {rvar, "r"}}]
                            }
              }
  }.

% PINTAN
bankauthsimple() ->
{rec, "t", {branch,
              [{payment, {assert, keyp,  {require, tb, {act,s_id, {act,r_tan,
                              {branch,[{tok,{assert,tan,{consume,tan,{act,r_details,{rvar, "t"}}}}},
                                       {tfail,{rvar,"t"}}]}}}}}},
                {statement,{act,s_statement,{rvar,"t"}}},
                {logout,endP}
               ]
            }
}.


keycard() -> {rec, "y", {require, keyp, {branch, [{tan, {assert, tb, {rvar, "y"}}},
                                                  {keycard, {rvar, "y"}}]}}}.


pin() ->
  {act, r_pin, {branch, [{ok, {assert, pin, endP}},
                              {fail, endP}]
                }
  }.

tan() ->
  {require, pin, {rec, "r", {act, s_id, {act, r_tan, {branch, [{ok, {assert, tan, {rvar, "r"}}},
                                                               {fail, {rvar, "r"}}]
                                                    }}}}}.

agent1() -> {branch, [{r_ua_set_ua_set, {assert, n, {assert, set, {act, r_ua_coord, {assert, coord, {act, s_au_state, endP}}}}}},
                      {r_ua_get, {assert, n, {assert, get,{act, s_au_snap, {assert, snap, endP}}}}},
                      {r_ua_close,{assert, n, {assert, close, endP}}}]
            }.

agent2() -> {consume, n, {branch, [{s_ai_set, {consume, set, {act, s_ai_coord, {consume, coord, {act, r_ia_state, endP}}}}},
                                   {s_ai_get, {consume, get, {act, r_ia_snap, {consume, snap, endP}}}},
                                   {s_ai_close, {consume, close, endP}}]
            }}.



userAgent() -> {rec, "r", {branch, [{ua_r_set, {act, ua_r_coord, {assert, set, {rvar, "r"}}}},
                                    {ua_r_get, {assert, get, {consume, snap, {act, au_s_snap, {rvar, "r"}}}}},
                                    {ua_r_close, {assert, close, endP}}
]}}.

agentInstrument() -> {rec, "t", {branch, [{ai_s_set, {consume, set, {act, ai_s_coord, {rvar, "t"}}}},
                                          {ai_s_get, {consume, get, {act, ai_r_snap, {assert, snap, {rvar, "t"}}}}},
                                          {ui_s_close, {consume, close, endP}}
]}}.

% Http server as described in literature
http() ->
  {act, r_request, {rec, "r", {branch,[{r_host,{rvar,"r"}},
                                      {r_usera,{rvar,"r"}},
                                      {r_acceptt,{rvar,"r"}},
                                      {r_acceptl,{rvar,"r"}},
                                      {r_acceptE,{rvar,"r"}},
                                      {r_dnt,{rvar,"r"}},
                                      {r_connection,{rvar,"r"}},
                                      {r_upgradeir,{rvar,"r"}},
                                      % {r_cookie,{rvar,"r"}},
                                      {r_body, reply()}]}}}.

reply() -> {require, auth, {act, s_httpv, {branch,[{'s_200', message()}, {'s_404', message()}]}}}.

message() -> {rec, "y", {branch, [{s_date,{rvar,"y"}},
                                {s_server,{rvar,"y"}},
                                {s_strictts,{rvar,"y"}},
                                {s_lastm,{rvar,"y"}},
                                {s_etag,{rvar,"y"}},
                                {s_acceptr,{rvar,"y"}},
                                {s_contentl,{rvar,"y"}},
                                {s_vary,{rvar,"y"}},
                                {s_contentt,{rvar,"y"}},
                                {s_via,{rvar,"y"}},
                                {s_cache,{rvar,"y"}},
                                {s_body, endP}]}}.

aws_auth() ->
  {rec, "x", {act, s_authenticate, {act, r_userpass,
        {branch, [{ok, {assert, auth, endP}},
                  {retry, {rvar, "x"}},
                  {forbidden, endP}]
                }}
  }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% two step email authentication
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% step one email address and password
auth_step_one() ->
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
auth_step_two() -> {rec, "scnd", {require, one, {branch, [{app, {act, prompt, {act, reply, {assert, auth, endP}}}},
                                            {code, {act, code, {act, reply, {assert, auth, endP}}}}
                                            ]}}}.
%% two step authentication
auth_two_step() ->
  {rec,"fst", {act,email, {branch,[{ok,{rec,"scnd", {act,password,
                                 {branch,[{ok,{assert,one,
                                                      {require,one,
                                                               {branch,[{app,{act,prompt,{act,reply,{assert,auth,endP}}}},
                                                                        {code,{act,code,{act,reply,{assert,auth,endP}}}}]}}}},
                                          {fail,{rvar,"scnd"}}]}}}},
                   {fail,{rvar,"fst"}}]}}}.


%% email menu
email() -> {rec, "menu", {require, auth, {branch, [{read, {act, read, {rvar, "menu"}}},
                                                   {send, {act, send, {rvar, "menu"}}},
                                                   {exit, {consume, auth, endP}}]}}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compute the number of interleavings for the different composition options
interleavings() ->
  Protocols = [{login(), service()},
               {services(), payments()},
               {payment(), dispatch()},
               {http(), aws_auth()},
               {login(), booking()},
               {pin(), tan()},
               {pintan(), bank()},
               {resource(), server()},
               {userAgent(), agentInstrument()},
               {bankauthsimple(), keycard()},
               % {auth_step_one(), auth_step_two()},
               {auth_two_step(), email()}],

  Strong = lists:map(fun({P1, P2}) -> length(interleave:interleave(P1, P2)) end, Protocols),
  Weak = lists:map(fun({P1, P2}) -> length(interleave:interleaveWeak(P1, P2)) end, Protocols),
  Correlating = lists:map(fun({P1, P2}) -> length(interleave:interleaveCorrelating(P1, P2)) end, Protocols),
  WeakCorrelating = lists:map(fun({P1, P2}) -> length(interleave:interleaveAll(P1, P2)) end, Protocols),
  io:format("Strong: ~p~n", [Strong]),
  io:format("Weak: ~p~n", [Weak]),
  io:format("Correlating: ~p~n", [Correlating]),
  io:format("Weak Correlating: ~p~n", [WeakCorrelating]).
