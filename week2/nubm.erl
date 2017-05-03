-module(nubm).
-export([nub/1, bun/1]).

nub([]) ->
  [];
nub([X|Xs]) ->
  [X|nub(removeAll(X,Xs))].

removeAll(_,[]) ->
  [];
removeAll(X,[X|Xs]) ->
  removeAll(X,Xs);
removeAll(X,[Y|Xs]) ->
  [Y|removeAll(X,Xs)].

bun([]) ->
  [];
bun([X|Xs]) ->
  case member(X,Xs) of
    true ->
      bun(Xs);
    false ->
      [X|bun(Xs)]
  end.

member(_,[]) ->
  false;
member(X,[X|_]) ->
  true;
member(X,[_|Xs]) ->
  member(X,Xs).
