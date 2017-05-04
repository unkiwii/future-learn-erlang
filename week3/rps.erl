-module(rps).
-export([test/0, tournament/2, result/2]).

beat(rock)     -> paper;
beat(paper)    -> scissors;
beat(scissors) -> rock;
beat(_)        -> error.

lose(rock)     -> scissors;
lose(paper)    -> rock;
lose(scissors) -> paper;
lose(_)        -> error.

result(A,B) ->
  result(lose(B),A,B).

result(error,_,_) ->
  {error, "Invalid choice"};
result(L,A,B) ->
  case beat(L) of
    A -> draw;
    _ -> not_draw(L,beat(A),B)
  end.

not_draw(_,_A,_A) -> lose;
not_draw(_A,_A,_) -> win.

tournament(_, []) -> {error,"No plays for right player"};
tournament([], _) -> {error,"No plays for left player"};
tournament(Left, Right) ->
  Games = lists:zip(Left, Right),
  Points = lists:map(fun play/1, Games),
  lists:foldr(fun sum/2, 0, Points).

play({Left, Right}) ->
  case result(Left, Right) of
    win         -> 1;
    lose        -> -1;
    draw        -> 0;
    E={error,_} -> E
  end.

sum(_,E={error,_}) -> E;
sum(E={error,_},_) -> E;
sum(A,B) -> A + B.

test() ->
  -1 = tournament([rock,rock,paper,paper], [rock,paper,scissors,rock]),
  ok.
