-module(list).
-import(shapes, [area/1]).
-export([is_in_list/2, median/1, mode/1, nub/1, circles/1, circles2/1, total_area/1, sumT/1, sum/1, second/1, head/1, tail/1]).

head([X|_]) -> X.
tail([_|Xs]) -> Xs.

second([_,Y|_]) -> Y.

sum([])     -> 0;
sum([X|Xs]) -> X + sum(Xs).

sumT(Xs) -> sumT(Xs, 0).
sumT([], S) -> S;
sumT([X|Xs], S) -> sumT(Xs, X+S).

total_area(Shapes) ->
  sum(areas(Shapes)).

areas([]) -> [];
areas([Shape|Shapes]) -> [ shapes:area(Shape) | areas(Shapes) ].

circles([]) -> [];
circles([{circle, {_,_}, _}=C | Shapes]) -> [ C | circles2(Shapes) ];
circles([_|Shapes]) -> [ circles2(Shapes) ].

circles2([]) -> [];
circles2([Shape|Shapes]) ->
  case Shape of
    {circle,{_,_},_}=C ->
      [ C | circles2(Shapes) ];
    _ ->
      circles2(Shapes)
  end.

nub([]) -> [];
nub([X]) -> [X];
nub([X|Xs]) ->
  case is_in_list(X, Xs) of
    true  -> nub(Xs);
    false -> [X | nub(Xs)]
  end.

is_in_list(_, []) -> false;
is_in_list(E, [X|Xs]) -> E == X orelse is_in_list(E, Xs).

%% TODO
median([X]) -> X;
median([_X|_Xs]) -> 0.

%% TODO
mode([X]) -> X;
mode([_X|_Xs]) -> 0.
