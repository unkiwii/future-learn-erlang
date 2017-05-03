-module(first).
-export([maxThree/3, howManyEqual/3, xOr1/2, xOr2/2, xOr3/2, xOr4/2, xOr5/2, is_zero/1, area/3, multiply/2, double/1, treble/1, square/1]).

multiply(X, Y) ->
  X * Y.

double(X) ->
  multiply(X, 2).

area(A, B, C) ->
  S = (A + B + C) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C)).

square(X) ->
  multiply(X, X).

treble(X) ->
  multiply(X, 3).

is_zero(0) ->
  true;
is_zero(_) ->
  false.


xOr1(true,false) ->
  true;
xOr1(false,true) ->
  true;
xOr1(X, X) ->
  false.

xOr2(X, X) ->
  false;
xOr2(_, _) ->
  true.

xOr3(A, B) ->
  A =/= B.

xOr4(A, B) ->
  not (A == B).

xOr5(A, B) ->
  (A or B) and (not (A and B)).

maxThree(X, Y, Z) ->
  max(max(X, Y), Z).

howManyEqual(X, X, X) -> 3;
howManyEqual(X, X, _) -> 2;
howManyEqual(X, _, X) -> 2;
howManyEqual(_, X, X) -> 2;
howManyEqual(_, _, _) -> 0.
