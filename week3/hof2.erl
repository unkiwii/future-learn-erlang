-module(hof2).
-export([zip/2, zip_hof/2, zip_with/3, zip_with_hof/3, test/0]).

zip(X, Y) ->
  zip(X, Y, []).

zip([], _, R) ->
  R;
zip(_, [], R) ->
  R;
zip([X|Xs], [Y|Ys], R) ->
  [{X, Y} | zip(Xs, Ys, R)].

zip_hof(X, Y) ->
  zip_with(X, Y, fun (A, B) -> {A, B} end).


zip_with(X, Y, F) ->
  zip_with(X, Y, F, []).

zip_with([], _, _, R) ->
  R;
zip_with(_, [], _, R) ->
  R;
zip_with([X|Xs], [Y|Ys], F, R) ->
  [F(X, Y) | zip_with(Xs, Ys, F, R)].

zip_with_hof(X, Y, F) -> lists:map(fun ({A, B}) -> F(A, B) end, zip(X, Y)).


test() ->
  test_zip(),
  test_zip_with(),
  test_fns2(fun zip/2, fun zip_hof/2),
  test_fns3(fun zip_with/3, fun zip_with_hof/3),
  ok.

sum(X, Y) ->
  X + Y.
concat(X, Y) ->
  X ++ Y.

test_data2() ->
  [
  %  expected           A           B
    {[],                [],         []},
    {[],                [1],        []},
    {[],                [],         [1]},
    {[],                [1, 2],     []},
    {[],                [],         [1, 2]},
    {[{1, 2}],          [1],        [2]},
    {[{1, 2}],          [1],        [2, 3]},
    {[{1, 2}],          [1, 3],     [2]},
    {[{a, b}],          [a],        [b]},
    {[{a, b}],          [a],        [b, c]},
    {[{a, b}],          [a, c],     [b]},
    {[{1, a}],          [1],        [a]},
    {[{1, a}],          [1],        [a, b]},
    {[{1, a}],          [1, 2],     [a]},
    {[{1, a}, {2, b}],  [1, 2],     [a, b]},
    {[{1, a}, {2, b}],  [1, 2, 3],  [a, b]},
    {[{1, a}, {2, b}],  [1, 2],     [a, b, c]}
  ].

test_data3() ->
  [
  % expected        A                 B                 transform
    {[],            [],               [],               fun sum/2},
    {[],            [1],              [],               fun sum/2},
    {[],            [],               [1],              fun sum/2},
    {[],            [1, 2],           [],               fun sum/2},
    {[],            [],               [1, 2],           fun sum/2},
    {[3],           [1],              [2],              fun sum/2},
    {[3],           [1],              [2, 3],           fun sum/2},
    {[3],           [1, 3],           [2],              fun sum/2},
    {[3, 7],        [1, 3],           [2, 4],           fun sum/2},
    {[3, 7],        [1, 3, 5],        [2, 4],           fun sum/2},
    {[3, 7],        [1, 3],           [2, 4, 6],        fun sum/2},
    {["ab"],        ["a"],            ["b"],            fun concat/2},
    {["ab"],        ["a"],            ["b", "c"],       fun concat/2},
    {["ab"],        ["a", "c"],       ["b"],            fun concat/2},
    {["ab", "cd"],  ["a", "c"],       ["b", "d"],       fun concat/2},
    {["ab", "cd"],  ["a", "c", "e"],  ["b", "d"],       fun concat/2},
    {["ab", "cd"],  ["a", "c"],       ["b", "d", "f"],  fun concat/2}
  ].

test_zip() ->
  Data = test_data2(),
  lists:foreach(fun ({Expected, A, B}) ->
                    Expected = zip(A, B)
                end, Data),
  ok.

test_zip_with() ->
  Data = test_data3(),
  lists:foreach(fun ({Expected, A, B, F}) ->
                    Expected = zip_with(A, B, F)
                end, Data),
  ok.

test_fns2(F, G) ->
  Data = test_data2(),
  lists:foreach(fun ({_, X, Y}) ->
                    Tmp = F(X, Y),
                    Tmp = G(X, Y)
                end, Data),
  ok.

test_fns3(F, G) ->
  Data = test_data3(),
  lists:foreach(fun ({_, X, Y, With}) ->
                    Tmp = F(X, Y, With),
                    Tmp = G(X, Y, With)
                end, Data),
  ok.
