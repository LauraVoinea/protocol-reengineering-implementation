-module(factorize).


%% @doc Factorization
fact({act, A, S1}, {act, A, S2}) -> fact(S1,S2);
fact({assert, A, S1}, {assert, A, S2}) ->  fact(S1,S2);
fact({consume, A, S1}, {consume, A, S2}) ->  fact(S1,S2);
fact({require, A, S1}, {require, A, S2}) ->  fact(S1,S2);
fact({act, A, S1}, S2) ->  {act, A, fact(S1,S2)};
fact({assert, A, S1}, S2) -> {assert, A, fact(S1,S2)};
fact({consume, A, S1}, S2) -> {consume, A, fact(S1,S2)};
fact({require, A, S1}, S2) -> {require, A, fact(S1,S2)};
fact({branch, LiSi } , {branch, RiSi}) ->
  L = bramatch(LiSi,RiSi),
  S = lists:last(L),
  case lists:all(fun(X) -> (X == S) end, L)  of
      true -> S;
      false -> L
  end;
fact({branch, LiSi}, S) ->
  {branch, interleave:for(LiSi, fun({A, R}) -> {A, fact(R, S)} end)};
fact(S, {rvar, _}) -> S;
fact({rvar, T}, _) -> {rvar, T};
fact(endP, _) -> endP;
fact(_, endP) -> endP.


bramatch([{A,S}],[{A,T}]) -> [fact(S,T)];
bramatch([{A,S}|B1],[{A,T}|B2]) -> [fact(S,T)] ++ bramatch(B1,B2);
bramatch(_,_)-> noP.
