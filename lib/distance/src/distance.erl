-module(distance).
-export([euklidean/2]).

euklidean_distance(A,B) ->
    math:pow(A - B, 2).

euklidean(Vector1,Vector2) ->
  math:sqrt(
    lists:sum(
      lists:zipwith(fun(X,Y) -> euklidean_distance(X,Y) end, Vector1, Vector2))).
