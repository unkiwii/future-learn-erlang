-module(types).
-export([take/2]).

-spec take(integer(), [T]) -> [T].

take(0, _) -> [];
take(_, []) -> [];
take(C, [X|Xs]) when C > 0 -> [X | take(C - 1, Xs)].
