-module(hof1).
-export([test/0]).

doubleAll([]) ->
  [];
doubleAll([X|Xs]) ->
  [2*X | doubleAll(Xs)].

doubleAll_hof(Xs) -> lists:map(fun (X) -> 2*X end, Xs).


evens([]) ->
  [];
evens([X|Xs]) when X rem 2 == 0 ->
  [X | evens(Xs)];
evens([_|Xs]) ->
  evens(Xs).

evens_hof(Xs) -> lists:filter(fun (X) -> X rem 2 == 0 end, Xs).


product([]) ->
  1;
product([X|Xs]) ->
  X * product(Xs).

product_hof(Xs) -> lists:foldr(fun (X,A) -> X * A end, 1, Xs).


test() ->
  Data = test_data(),
  test_fns(fun doubleAll/1, fun doubleAll_hof/1, Data),
  test_fns(fun evens/1, fun evens_hof/1, Data),
  test_fns(fun product/1, fun product_hof/1, Data),
  ok.

test_data() ->
  [
   [],
   [0],
   [1],
   [2],
   [0, 1, 2, 3, 4],
   [4, 3, 2, 1, 0],
   [1, 2, 3, 4],
   [4, 3, 2, 1],
   [1, 3, 5, 7],
   [2, 4, 6, 8]
  ].

test_fns(F, G, Data) ->
  lists:foreach(fun (D) ->
                    Tmp = F(D),
                    Tmp = G(D)
                end, Data).
