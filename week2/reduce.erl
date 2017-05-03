-module(reduce).
-export([test/0]).

reduce(Xs) ->
  reduce_(remove_duplicates(Xs)).

reduce_([]) ->
  [];
reduce_([X|Xs]) ->
  %TODO: implement this
  [X|Xs].

remove_duplicates([]) ->
  [];
remove_duplicates([X]) ->
  [X];
remove_duplicates([X|Xs]) ->
  case lists:member(X, Xs) of
    true  -> remove_duplicates(Xs);
    false -> [X | remove_duplicates(Xs)]
  end.

test() ->
  [] = reduce([]),
  [1] = reduce([1]),
  [2] = reduce([2]),
  [1] = reduce([1,1]),
  [2] = reduce([2,2]),
  [1,2] = reduce([1,2]),
  ok.
