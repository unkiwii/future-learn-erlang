-module(recursion).
-export([isok/1,factorial/1, fib/1, pieces/1]).

factorial(0) ->
  1;
factorial(N) when N > 0 ->
  N * factorial(N - 1).

fib(0) ->
  0;
fib(1) ->
  1;
fib(N) when N > 1 ->
  fib(N - 1) + fib(N - 2).

pieces(0) ->
  1;
pieces(N) when N > 0 ->
  pieces(N - 1) + N.

isok({ok,_}) ->
  yes;
isok({_,_}) ->
  no.
