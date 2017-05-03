-module(tail).
-export([fac/1, fib/1, perfect/1]).

fac(N) -> fac(N, 1).

fac(0, P) -> P;
fac(N, P) -> fac(N - 1, P * N).

fib(N) -> fib(N, 0, 1).

fib(0, P, _) -> P;
fib(N, P, C) when N > 1 -> fib(N - 1, C, P + C).

perfect(N) -> perfect(N, N - 1, 0).

perfect(N, 0, S) -> N == S;
perfect(N, D, S) when N rem D > 0 -> perfect(N, D - 1, S);
perfect(N, D, S) when N rem D == 0 -> perfect(N, D - 1, S + D).

%% Another way to calculate perfect numbers using list comprenhension:
% perfect(N) -> N == sum([D || D <- lists:seq(1, N div 2), N rem D == 0]).
% sum(L) -> sum(L, 0).
% sum([H|T], S) -> sum(T, S + H);
% sum([], S)    -> S.
