%% @doc Interleaving composition and decomposition of protocols
-module(interleave).
-compile(export_all).
-compile(nowarn_export_all).

%% @doc Protocol format
%% Notes:
%%   - Recursion variables are represented as string()
%%   - Actions, labels, and assertion names are represented as atom()
%%   - Control branches are represented as a list of atom/protocol pairs
%%   - The atom endP is used to end a protocol because "end" is a reserved keyword.
-type protocol () :: {'act', atom(), protocol()}
                   | {'branch', [ {atom(), protocol()} ]}
                   | {'assert', atom(), protocol()}
                   | {'require', atom(), protocol()}
                   | {'consume', atom(), protocol()}
                   | {'rec', string(), protocol()}
                   | {'rvar', string()}
                   | 'endP'.


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
  {act, r_pin, {branch, [
                          {ok, {assert, pin, {rec, r, {consume, pay, ctan()}}}},
                          {fail, endP}]
                }
  }.

ctan() ->
   {act, s_id, {act, r_tan, {branch, [{tok, {assert, tan, {rvar, r}}},
                                            {tfail, {rvar, r}}]
                            }
              }
  }.

bankauth() ->
{act,r_pin,
 {branch,
  [{ok,
    {assert,pin,
     {require,pin,
      {rec,t,
       {require, keyp, 
       {branch,
        [{payment,
          {assert,pay,
           {consume,pay,
            {act,s_id,
             {act,r_tan,
              {branch,
               [{tok,{assert,tan,{consume,tan,{act,r_details,{rvar,t}}}}},
                {tfail,{rvar,t}}]}}}}}},
         {statement,{act,s_statement,{rvar,t}}},
         {logout,{consume,pin,endP}}]}}}}}},
   {fail,endP}]}
   }.
   
bankauthsimple() ->
{act,r_pin,
 {branch,
  [{ok,
      {rec,t,
          {branch,
              [ {payment, {assert, keyp, {require, tb, {act,s_id, {act,r_tan, {branch,
                                      [  {tok,{assert,tan,{consume,tan,{act,r_details,{rvar,t}}}}},
                                          {tfail,{rvar,t}}
                                      ]}}
                          }
                }}},
                {statement,{act,s_statement,{rvar,t}}},
                {logout,{consume,pin,endP}}]
            }
        }},
    {fail,endP}
  ]
   }}.

keycard() -> {
{rec, y, {require, keyp, {branch, [{tan, {assert, tb, {rvar, y}}},
                                  {keycard, {rvar, y}}
                                  ]
                        }}
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

  


%% @doc Pretty print protocols
-spec pprint(protocol()) -> string().
pprint({act, Act, P}) ->
  atom_to_list(Act) ++ "." ++ pprint(P);
pprint({branch, Branches}) ->
  "{" ++ pprintBranches(Branches) ++ "}";
pprint({assert, N, P}) ->
  "assert(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({require, N, P}) ->
  "require(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({consume, N, P}) ->
  "consume(" ++ atom_to_list(N) ++ ")." ++ pprint(P);
pprint({rec, BoundVar, P}) ->
  "nu " ++ BoundVar ++ " . (" ++ pprint(P) ++ ")";
pprint({rvar, Var}) ->
  Var;
pprint(endP) ->
  "end".
  
% power set

power([]) -> [[]];
power([H|T]) -> PT = power(T),
   [ [H|X] || X <- PT ] ++ PT .
  
  
filterSet(Data) when is_list(Data) ->
    Pred = fun(Element) -> Element /= [] end,
    lists:filter(Pred, Data). 


    

% Finds the subset of J without empty set
jBranch(J) -> filterSet(power(J)).

% Returns true if a bad combo i.e., it has at least an empty branch
badJCombo1(A) ->
  Results = for(A, fun({_,{branch, Si}}) ->
      case Si of
        [] -> true;
         _ -> false
      end
    end),
  lists:member(true, Results).
    
% Returns true if a bad combo i.e., there is an element in I that is not in any branch Ji
badJCombo2(A, I) ->
  Indices = for(A, fun({_,{branch, Js}}) ->
            for(Js, fun({J, _}) -> J
            end)
          end),
  case lists:usort(lists:flatten(Indices)) =:= lists:usort(lists:flatten(I)) of
    true -> false;
    _ -> true
  end.





com1() -> {branch, [{a, {consume, a, endP}}, {b, {consume, b, endP}}, {c, endP} ] }.
com2() -> {branch, [{aa, {assert, a, endP}}, {bb, {assert, b, endP}}] }.

test({branch, LiSi2}) -> jBranch(LiSi2).



%% @doc Strip assertions
-spec strip(protocol()) -> protocol().
strip({act, N, P}) -> {act, N, strip(P)};
strip({assert, _, P}) -> strip(P);
strip({require, _, P}) -> strip(P);
strip({consume, _, P}) -> strip(P);
strip({branch, LiSi}) -> 
  {branch, for(LiSi, fun({Li, Si}) -> {Li, strip(Si)} end)};
strip({rec, BV3, P}) -> {rec, BV3, strip(P)};
strip(P) -> P. 

stripSet([]) -> [];
stripSet([X|XX]) -> [strip(X)] ++ stripSet(XX).



%% @doc Substitution
-spec subst(protocol(), string(), string(), [string()]) -> protocol().
subst({act, Act, P}, BV1, BV2, A) -> {act, Act, subst(P, BV1, BV2, A)};
subst({assert, N, P}, BV1, BV2, A) -> {assert, N, subst(P, BV1, BV2, A)};
subst({require, N, P}, BV1, BV2, A) -> {require, N, subst(P, BV1, BV2, A)};
subst({consume, N, P}, BV1, BV2, A) -> {consume, N, subst(P, BV1, BV2, A)};
subst({branch, LiSi}, BV1, BV2, A) ->
  {branch, for(LiSi, fun({Li, Si}) -> {Li, subst(Si, BV1, BV2, A)} end)};
subst({rec, BV3, P}, BV1, BV2, A) ->
  case lists:member(BV1, A) of
    true -> {rec, BV3, subst(P, BV1, BV2, A)};
    false -> {rec, BV3, subst(P, BV1, BV2, A ++ [BV3])}
  end;
subst({rvar, BV1}, BV1, BV2, A) ->
  case lists:member(BV1, A) of
    true -> {rvar, BV1};
    false -> {rvar, BV2}
  end;
subst({rvar, BV3}, _, _, _) -> {rvar, BV3};
subst(endP, _ , _ , _ ) -> endP.

%% @doc Auxiliary printers
%% Prints a branch
pprintBranch({Label, P}) -> atom_to_list(Label) ++ " : " ++ pprint(P).
% Prints a list of branches
pprintBranches([])     -> "";
pprintBranches([B])    -> pprintBranch(B);
pprintBranches([B|BS]) -> pprintBranch(B) ++ "; " ++ pprintBranches(BS).

%% @doc Assertedness
% WIP: defaults to well-asserted
-spec asserted([atom()], protocol()) -> [atom()] | 'illAsserted'.
asserted(A , endP) -> A;
asserted(A, {rvar, _}) -> A;
asserted(A, {act, _, P}) -> asserted(A, P);
asserted(A, {branch, LiSi}) ->
  Abranches = for(LiSi, fun({_,Si}) -> asserted(A, Si) end),
  case listAsserted(Abranches) of
    true -> listIntersect(Abranches);
    false -> 'illAsserted'
  end;
asserted(A, {require, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(A, P);
    false -> 'illAsserted'
  end;
asserted(A, {consume, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(lists:delete(N,A), P);
    false -> 'illAsserted'
  end;
asserted(A, {assert, N, P}) ->
  case lists:member(N, A) of
    true -> asserted(A, P);
    false -> asserted(A ++ [N], P)
  end;
asserted(A, {rec, _, P}) ->
  case asserted(A, P) of
    illAsserted -> illAsserted;
    B -> lists:usort(B ++ A)
  end.
wellAsserted(A, PS) ->
  case asserted(A, PS) of
    illAsserted -> false;
    _           -> true
  end.

%% @doc Helper functions for assertedness
listAsserted([A|Alist]) ->
  case A of
    illAsserted -> false;
    _ -> listAsserted(Alist)
  end;
listAsserted([]) -> true.

listIntersect(A) ->
  sets:to_list(sets:intersection(for(A, fun(X) -> sets:from_list(X) end))).

%% @doc Helper functions of binders
%%Predicate on whether protocol P has all its free variables in environment N
bound(P, N) ->
  case P of
    {act, _, R} -> bound(R,N);
    {assert, _, R} -> bound(R,N);
    {require, _, R} -> bound(R,N);
    {consume, _, R} -> bound(R,N);
    {branch, LiSi} -> lists:all(fun(X) -> X end, for(LiSi, fun({_, Si})-> bound(Si,N) end) );
    {rec, T, R} -> bound(R,N ++ [T]);
    {rvar, T} -> lists:member(T,N);
    endP -> true
  end.

%% @doc Interleaving
%% Helper for plumbing non-determinstic results (represented as lists)
%% into functions which are non-determinstic (return a list of results)
-spec bind([A], fun((A) -> [B])) -> [B].
bind([], _F)    -> [];
bind([X|XS], F) -> F(X) ++ bind(XS, F).

%% @doc Basically just flip map
-spec for([A], fun((A) -> B)) -> [B].
for(XS, F) -> lists:map(F, XS).

%% @doc Remove duplicate elements
-spec nub([A]) -> [A].
nub(X) -> nub(X, []).
nub([], Clean) -> Clean;
nub([X|Xs], Clean) ->
    case lists:member(X, Clean) of
        true -> nub(Xs, Clean);
        false -> nub(Xs, Clean ++ [X])
    end.

%% @doc Compute a covering of a set (with two partitions)
twoCovering([])  -> [];
twoCovering([A]) -> [{[A], []}, {[], [A]}];
twoCovering([A|AS]) ->
  bind(twoCovering(AS), fun({XS, YS}) -> [{[A|XS], YS}, {XS, [A|YS]}] end).





%% @doc Take the largest list in a list of lists
maximalPossibility(XS) -> maximalPoss(XS, []).
maximalPoss([], Max) -> Max;
maximalPoss([XS|XSS], Max) when length(XS) >= length(Max) -> maximalPoss(XSS, XS);
maximalPoss([_|XSS], Max)  -> maximalPoss(XSS, Max).

% Top-level
-spec interleave(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleave(S1, S2) -> nub(interleaveTop(strong, [], [], [], S1, S2)).

-spec interleaveWeak(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveWeak(S1, S2) -> nub(interleaveTop(weak, [], [], [], S1, S2)).

-spec interleaveAll(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveAll(S1, S2) ->
    nub(interleaveTop(all, [], [], [], S1, S2)).

-spec interleaveCorrelating(protocol(), protocol()) -> [protocol ()].
%% @doc Wraps the main function and passes in empty environments
interleaveCorrelating(S1, S2) ->
    nub(interleaveTop(correlating, [], [], [], S1, S2)).


%% @doc n-way Cartesian product
-spec nCartesian([[A]]) -> [[A]].
nCartesian([]) -> [];
%% XS is one list, [XS] is the list of one list of lists
nCartesian([XS]) -> lists:map(fun (X) -> [X] end, XS);
nCartesian([XS|XSS]) ->
  bind(XS, fun(X) -> bind(nCartesian(XSS), fun(YS) -> [[X|YS]] end) end).

%% @doc Takes
%%   - a list TL of recursion variables [string()] bound on the left
%%   - a list TR of recursion variables [string()] bound on the right
%%   - a list of atoms for the asserted names
%%   - left protocol
%%   - right protocol
%% This function should be used in all recursive calls since it also implements
%% the symmetry rule, where as interleaveMain does the main, asymmetrical work
-spec interleaveTop(atom(), [string()], [string()], [atom()], protocol(), protocol()) -> [protocol()].
%% [sym] rule
interleaveTop(WeakFlag, TL, TR, A, S1, S2) ->
  interleaveMain(WeakFlag, TL, TR, A, S1, S2) ++
    interleaveMain(WeakFlag, TR, TL, A, S2, S1).

%% @doc Asymmetrical (left-biased) rules
-spec interleaveMain(atom(), [string()], [string()], [atom()], protocol(), protocol()) -> [protocol()].
%% [end] rule
interleaveMain(_, _, _, _, endP, endP) -> [endP];
%% [act] rule
interleaveMain(WeakFlag, TL, TR, A, {act, P, S1}, S2) ->
  for(interleaveTop(WeakFlag, TL, TR, A, S1, S2), fun(S) -> {act, P, S} end);
%% [require] rule
interleaveMain(WeakFlag, TL, TR, A, {require, N, S1}, S2) ->
  case lists:member(N, A) of
    true ->
      % Induct
      for(interleaveTop(WeakFlag, TL, TR, A, S1, S2)
        , fun(S) -> {require, N, S} end);
    false -> [] % Fail
  end;
%% [consume] rule
interleaveMain(WeakFlag, TL, TR, A, {consume, N, S1}, S2) ->
  case lists:member(N, A) of
    true ->
      % Induct
      for(interleaveTop(WeakFlag, TL, TR, lists:delete(N, A), S1, S2)
        , fun(S) -> {consume, N, S} end);
    false -> [] % Fail
  end;
%% [assert] rule
interleaveMain(WeakFlag, TL, TR, A, {assert, P, S1}, S2) ->
  for(interleaveTop(WeakFlag, TL, TR, [P|A], S1, S2)
      , fun(S) -> {assert, P, S} end);

%% [bra] rule
%% if for branches S0, S1, S2 we get the following possible interleavings with S2
%%   S0'_0, S0'_1
%%   S1'_0, S1'_1, S1'_2
%%   S2'_0, S2'_1, S3'_2
%% then nCartesian takes all possible combinations

%% LiSi is the list of label-protocol pairs
interleaveMain(_, _, _, _, {branch, []}, _) -> errorEmptyBranch;


interleaveMain(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ->
  case WeakFlag of
    strong -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}));
    weak -> lists:usort(intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}));
    correlating -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2})  ++ intCorrelating(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}));
    all -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2})  ++ intCorrelating(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ++ intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, {branch, LiSi2}))
  end;  


interleaveMain(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2) ->
  case WeakFlag of
    strong -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2));
    weak -> lists:usort(intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2));
    correlating -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2));
    all -> lists:usort(intStrong(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2)  ++ intWeak(WeakFlag,  TL, TR, A, {branch, LiSi1}, S2))
  end;  


  
%% [rec1]
interleaveMain(WeakFlag, TL, TR, A, {rec, BV1, S1}, {rec, BV2, S2}) ->
  % Top(S1) not a recursion
  case S1 of
    {rec, _, _} -> [];
    _ -> for(
          interleaveTop(WeakFlag, TL ++ [BV1], TR, A, S1, {rec, BV2, S2})
          , fun(S) ->
          case wellAsserted(A, {rec, BV1, S}) of
                        true -> {rec, BV1, S};
                        false -> []
                      end
            end)
  end;
  
  
 %% [rec3]
interleaveMain(_, _, _, A, {rec, BV1, S1}, endP) ->
  case wellAsserted(A, {rec, BV1, S1}) and bound({rec, BV1, S1},[]) of
    true -> [{rec, BV1, S1}];
    false -> []
  end;
 %% [rec2]
interleaveMain(WeakFlag, TL, TR, A, {rec, BV1, S1}, S2) ->
  case S1 of
    % TOP check
    {rec, _, _} -> [];
    _ -> lists:append(for(TR, fun(S)->
            interleaveTop(WeakFlag, TL, TR, A, subst(S1, BV1, S, []), S2) end))
  end;
%% [call]
interleaveMain(_, TL, TR , _, {rvar, BV1}, {rvar, BV1}) ->
  case lists:member(BV1, TL) or lists:member(BV1, TR) of
    true -> [{rvar, BV1}];
    false -> []
  end;
%% check top and well assertedness
interleaveMain(_, _, _, _, _, _) -> [].


 %% [bra] 
intStrong(WeakFlag, TL, TR, A, {branch, LiSi}, S2) ->
  Covering = [{LiSi, []}],
  Possibilities = for(Covering,
    fun ({Ia, Ib}) ->
    % Good parition if all Sb are well asserted
    case lists:all(fun ({_, Sib}) -> wellAsserted(A, Sib) end, Ib) of
      % Good parition
      true -> AllCombinations = nCartesian(for(Ia,
                    fun ({Li, Si}) ->
                    % Find all intereleavings for Si with S2 - put with its label
                    % with possible weakening modes
                    for(interleaveTop(WeakFlag, TL, TR, A, Si, S2),
                          fun(Sip) -> {Li, Sip} end)
                    end)),
        for(AllCombinations, fun(LiSip) -> {branch, LiSip ++ Ib} end);

      % Bad partition Ib is not all well-asserted
      false -> []
    end
  end),
  lists:usort(lists:concat(Possibilities)).

 
  %% [wbra]
intWeak(WeakFlag, TL, TR, A, {branch, LiSi}, S2) ->
  Covering = lists:droplast(twoCovering(LiSi)),
  Possibilities = for(Covering,
    fun ({Ia, Ib}) ->
    % Good parition if all Sb are well asserted
    case lists:all(fun ({_, Sib}) -> wellAsserted(A, Sib) end, Ib) of
      % Good parition
      true -> AllCombinations = nCartesian(for(Ia,
                    fun ({Li, Si}) ->
                    % Find all intereleavings for Si with S2 - put with its label
                    % with possible weakening modes
                    for(interleaveTop(WeakFlag, TL, TR, A, Si, S2),
                          fun(Sip) -> {Li, Sip} end)
                    end)),
        for(AllCombinations, fun(LiSip) -> {branch, LiSip ++ Ib} end);

      % Bad partition Ib is not all well-asserted
      false -> []
    end
  end),
  maximalPossibility(Possibilities).


%% [cbra]
intCorrelating(WeakFlag, TL, TR, A, {branch, LiSi1}, {branch, LiSi2}) ->
  I = for(LiSi2, fun({Li, _}) -> Li end),
  RightSubsets = jBranch(LiSi2),
  % Meat
  LeftAndRightSubsetCombos =
     % For each {li : Si}
     for(LiSi1, fun ({Li, Si}) ->
        % For each subset of the {lj , Sj} branches
        for(RightSubsets, fun (Subset) ->
          % associate with Li a branch...
          {Li, {branch,
                 %... all the possibile unique {lj, Sj} pairs where Si and Sj compose
                 nub([{Lj, S} || {Lj, Sj} <- Subset, S <- interleaveMain(WeakFlag, TL, TR, A, Si, Sj)])}}
        end)
      end),
  % Now choose all combiations across branches
  Results = for(nCartesian(LeftAndRightSubsetCombos), fun (Branches) ->
              % check that inner branching is non empty for all Li (in the paper Ji =\= 0 )
              case badJCombo1(Branches) of
                true -> [];
                % check all branches of J are covered in the I branches overall (in the paper U_{j\in J} = J)
                false ->  case badJCombo2(Branches, I) of
                            true -> [];
                            false -> {branch, Branches}
                          end 
              end
 end),
%remove empty list
lists:filter(fun(X) -> X /= [] end, Results).


% Factorization - ongoing work
%[Fprex1]
fact({act, A, S1}, {act, A, S2}) ->
  fact(S1,S2);

fact({assert, A, S1}, {assert, A, S2}) ->
  fact(S1,S2);

fact({consume, A, S1}, {consume, A, S2}) ->
  fact(S1,S2);

fact({require, A, S1}, {require, A, S2}) ->
  fact(S1,S2);

%[Fprex2]
fact({act, A, S1}, S2) ->
  {act, A, fact(S1,S2)};

fact({assert, A, S1}, S2) ->
  {assert, A, fact(S1,S2)};

fact({consume, A, S1}, S2) ->
  {consume, A, fact(S1,S2)};

fact({require, A, S1}, S2) ->
  {require, A, fact(S1,S2)};

%[Fbra1] with I = J
fact({branch, LiSi } , {branch, RiSi}) ->
  L = bramatch(LiSi,RiSi),
  S = lists:last(L),
  case lists:all(fun(X) -> (X == S) end, L)  of
      true -> S;
      false -> L
  end;


%[Fbra2]
fact({branch, LiSi } , S) ->
  {branch , for(LiSi, fun({A,R}) -> {A,fact(R,S)} end) };


fact({rec, T1, S1}, {rec, T1, S2}) -> fact(S1,S2);

fact({rec, T1, S1}, {rec, T2, S2}) -> fact(S1,subst(S2, T1, T2, []));

fact({rec, T, S}, _) -> {rec, T, S};


fact({rvar, T1}, {rvar, T1}) -> {rvar, T1};


fact(S, {rvar, _}) -> S;

fact(endP, _) -> endP;

fact(_, endP) -> endP.

bramatch([{A,S}],[{A,T}]) -> [fact(S,T)];
bramatch([{A,S}|B1],[{A,T}|B2]) -> [fact(S,T)] ++ bramatch(B1,B2);
bramatch(_,_)-> noP.