-module(adv_list).
-export([test/0, join/2, concat/1, member/2, msort/1, qsort/1, isort/1, permutations/1]).

join([], T) -> T;
join([X|Xs], T) -> [X | join(Xs,T)].

concat([]) -> [];
concat([X|Xs]) -> join(X, concat(Xs)).

member(_X, []) -> false;
member(_X, [_X|_]) -> true;
member(X, [_|Xs]) -> member(X, Xs).

msort([]) -> [];
msort([X]) -> [X];
msort(Xs) ->
  {Lo, Hi} = lists:split(length(Xs) div 2, Xs),
  merge(msort(Lo), msort(Hi)).

merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X|Xs], [Y|Ys]) when X =< Y ->
  [X|merge(Xs, [Y|Ys])];
merge([X|Xs], [Y|Ys]) when X > Y ->
  [Y|merge([X|Xs], Ys)].

qsort([]) -> [];
qsort([X|Xs]) ->
  qsort([N || N <- Xs, N < X]) ++ [X] ++ qsort([N || N <- Xs, N >= X]).

isort([]) -> [];
isort(Xs) -> lists:foldl(fun insert/2, [], Xs).

insert(X, []) -> [X];
insert(X, Xs=[H|_]) when X =< H -> [X|Xs];
insert(X, [H|T]) -> [H|insert(X, T)].

permutations([]) -> [[]];
permutations(Xs)  -> [[X|Ys] || X <- Xs, Ys <- permutations(Xs -- [X])].

test() ->
  test_join(),
  test_concat(),
  test_member(),
  test_sort(fun msort/1),
  test_sort(fun qsort/1),
  test_sort(fun isort/1),
  test_permutations(),
  ok.

test_join() ->
  [] = join([], []),
  [1] = join([1], []),
  [1] = join([], [1]),
  [1,2] = join([1], [2]),
  [1,2,3] = join([], [1,2,3]),
  [1,2,3] = join([1], [2,3]),
  [1,2,3] = join([1,2], [3]),
  [1,2,3] = join([1,2,3], []),
  [1,2,3,4] = join([], [1,2,3,4]),
  [1,2,3,4] = join([1], [2,3,4]),
  [1,2,3,4] = join([1,2], [3,4]),
  [1,2,3,4] = join([1,2,3], [4]),
  [1,2,3,4] = join([1,2,3,4], []),
  ok.

test_concat() ->
  [] = concat([]),
  "" = concat(""),
  "a" = concat(["a"]),
  "a" = concat(["a", ""]),
  "a" = concat(["", "a"]),
  "abc" = concat(["abc", ""]),
  "abc" = concat(["ab", "c"]),
  "abc" = concat(["a", "bc"]),
  "abc" = concat(["", "abc"]),
  "goodbye" = concat(["good","bye"]),
  [1, 2, 3, 4] = concat([[1, 2, 3, 4], []]),
  [1, 2, 3, 4] = concat([[1, 2, 3], [4]]),
  [1, 2, 3, 4] = concat([[1, 2], [3, 4]]),
  [1, 2, 3, 4] = concat([[1], [2, 3, 4]]),
  [1, 2, 3, 4] = concat([[], [1, 2, 3, 4]]),
  ok.

test_member() ->
  false = member(0, []),
  false = member(0, [1]),
  true = member(0, [0]),
  false = member(0, "hola"),
  false = member(0, "1023"),
  true = member(0, [0]),
  true = member(0, [1, 0, 2, 3]),
  ok.

test_sort(Sort) ->
  [] = Sort([]),
  [1] = Sort([1]),
  [1,2] = Sort([1,2]),
  [1,2] = Sort([2,1]),
  [10,20,30,40,50] = Sort([10,20,30,40,50]),
  [10,20,30,40,50] = Sort([50,30,20,10,40]),
  S1 = Sort([1,2,3]), S2 = Sort([3,2,1]), S1 = S2,
  C1 = Sort("hola mundo"), C2 = Sort("mundo hola"), C1 = C2,
  ok.

test_permutations() ->
  [[]] = permutations([]),
  [[1]] = permutations([1]),
  [[1,2],[2,1]] = lists:sort(permutations([1,2])),
  [[1,2],[2,1]] = lists:sort(permutations([2,1])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([1,2,3])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([1,3,2])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([2,1,3])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([2,3,1])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([3,1,2])),
  [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] = lists:sort(permutations([3,2,1])),
  ["a"] = lists:sort(permutations("a")),
  ["ab","ba"] = lists:sort(permutations("ab")),
  ["ab","ba"] = lists:sort(permutations("ba")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("abc")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("acb")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("bac")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("bca")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("cab")),
  ["abc","acb","bac","bca","cab","cba"] = lists:sort(permutations("cba")),
  ok.
