-module(second).
-import(first, [square/1, area/3]).
-export([hypotenuse/2, perimeter/2, area/2]).

hypotenuse(S1, S2) ->
  % h^2 = c1^2 + c^2
  C1 = first:square(S1),
  C2 = first:square(S2),
  math:sqrt(C1 + C2).

perimeter(S1, S2) ->
  hypotenuse(S1, S2) + S1 + S2.

area(S1, S2) ->
  fist:area(hypotenuse(S1, S2), S1, S2).
