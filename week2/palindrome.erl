-module(palindrome).
-export([valid/1]).

-spec valid([term()]) -> boolean().

valid(Xs) ->
  Xs == lists:reverse(Xs).
