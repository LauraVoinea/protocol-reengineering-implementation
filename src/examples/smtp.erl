-module(smtp).
-compile(export_all).
-compile(nowarn_export_all).



smtp() -> {act, '220', {branch, [{ehlo, {rec, "x", {branch, [{'250d', {rvar, "x"}},
                                                             {'250', start_tls()}]}}},
                                  {quit, endP}]}}.
% smtp() -> {act, '220', {branch, [{ehlo, endP},
%                                   {quit, endP}]}}.

start_tls() -> {branch, [{startTls, {act, '220', secure_ehlo()}},
                         {quit, endP}]}.

secure_ehlo() -> {branch, [{ehlo, {rec, "y", {branch, [{'250d', {rvar, "y"}},
                                                       {'250', auth()}]}}},
                           {quit, endP}]}.

% auth() -> {rec, "z", {branch, [{auth, {branch, [{'235', mail()}, {'535', {rvar, "z"}}]}},
%                                {quit, endP}]}}.

auth() -> {rec, "z", {branch, [{auth, {branch, [{'235', endP}, {'535', {rvar, "z"}}]}},
                              {quit, endP}]}}.

% mail() -> {rec, "a", }
