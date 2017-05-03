-module(ex2).
-export([modes/1, median/1, double/1, evens/1]).

double([]) -> [];
double([X|Xs]) -> [X * 2 | double(Xs)].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 -> [X | evens(Xs)];
evens([_X|Xs]) -> evens(Xs).

median([]) -> [];
median(Xs) ->
  Sorted = lists:sort(Xs),
  Size = length(Xs),
  Half = Size div 2,
  case Size rem 2 of
    0 -> (lists:nth(Half, Sorted) + lists:nth(Half + 1, Sorted)) / 2;
    1 -> lists:nth(Half + 1, Sorted)
  end.
