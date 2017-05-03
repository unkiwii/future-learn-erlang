-module(ex1).
-export([maximum/1, product/1, tmaximum/1, tproduct/1]).

tproduct(Xs) -> tproduct(Xs, 1).

tproduct([], P) -> P;
tproduct([X|Xs], P) -> tproduct(Xs, X * P).

tmaximum([X|Xs]) -> tmaximum(Xs, X).

tmaximum([], M) -> M;
tmaximum([X|Xs], M) -> tmaximum(Xs, max(M,X)).

product([]) -> 1;
product([X|Xs]) -> X * product(Xs).

maximum([X]) -> X;
maximum([X|Xs]) -> max(X, maximum(Xs)).
