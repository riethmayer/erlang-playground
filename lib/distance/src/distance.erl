-module(distance).
-export([chebyshev/2,
         cosine/2,
         cosine_similarity/2,
         euklid/1, euklid/2,
         hamming/2,
         haversine/2, haversine/3,
         jaccard/2 ]).
-define(EARTH_RADIUS_IN_MILES, 3956).
-define(EARTH_RADIUS_IN_KILOMETERS, 6372.8).

%% euklidian distance in N dimensions
%% http://en.wikipedia.org/wiki/Euclidean_distance
euklid(Vector1,Vector2) ->
    Distance = fun (A,B) -> math:pow(A - B, 2) end,
    math:sqrt(
      lists:sum(
        lists:zipwith(Distance, Vector1, Vector2))).

%% euklidean norm
%% http://en.wikipedia.org/wiki/Euclidean_norm#Euclidean_norm
euklid(Vector) ->
    NormVector = [ 0 || _ <- lists:seq(1, length(Vector))],
    euklid(Vector,NormVector).

dotproduct(V1,V2) ->
    Multiply = fun (A,B) -> A*B end,
    lists:sum(lists:zipwith(Multiply, V1, V2)).

%% chebychev distance
%% http://en.wikipedia.org/wiki/Chebyshev_distance
chebyshev(V1,V2) ->
    Distance = fun (A,B) -> abs(A-B) end,
    lists:max(lists:zipwith(Distance,V1, V2)).

%% cosine distance
%% http://en.wikipedia.org/wiki/Cosine_similarity#cosine_distance
cosine(U,V) ->
    1 - cosine_similarity(U,V).

%% cosine similarity
%% http://en.wikipedia.org/wiki/Cosine_similarity
cosine_similarity(U,V) ->
    dotproduct(U,V) / (euklid(U) * euklid(V)).

%% hamming distance
%% http://en.wikipedia.org/wiki/Hamming_distance
hamming(S,T) ->
    lists:sum([ 1 || {A,B} <- lists:zip(S,T), A =/= B ]).

%% haversine distance
%% http://en.wikipedia.org/wiki/Haversine_formula
haversine(P1,P2,Radius) ->
    {Lat1,Lon1} = P1,
    {Lat2,Lon2} = P2,
    LatDist = haversin(Lat2 - Lat1),
    LonDist = haversin(Lon2 - Lon1),
    N = LatDist + math:cos(radiant(Lat1)) * math:cos(radiant(Lat2)) * LonDist,
    {Radius, 2 * earth_radius(Radius) * math:asin(math:sqrt(N))}.

haversine(P1,P2) ->
    haversine(P1,P2,kilometers).
radiant(N) ->
    N * math:pi()/180.
haversin(N) ->
    math:pow(math:sin(radiant(N)/2),2).
earth_radius(miles) ->
    ?EARTH_RADIUS_IN_MILES;
earth_radius(kilometers) ->
    ?EARTH_RADIUS_IN_KILOMETERS;
earth_radius(feet) ->
    ?EARTH_RADIUS_IN_MILES * 5282;
earth_radius(meters) ->
    ?EARTH_RADIUS_IN_KILOMETERS * 1000.

%% jaccard index
%% http://en.wikipedia.org/wiki/Jaccard_index
jaccard(A,B) when is_list(A) and is_list(B) ->
    jaccard(sets:from_list(A), sets:from_list(B));
jaccard(A,B) ->
    sets:size(sets:intersection(A,B)) / sets:size(sets:union(A,B)).

