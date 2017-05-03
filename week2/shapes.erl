-module(shapes).
-export([perimeter/1, area/1, enclose/1]).

perimeter({circle, {_X, _Y}, R}) ->
  2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, W, H}) ->
  2 * W + 2 * H;
perimeter({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  [S1, S2, S3] = triangle_sides({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}),
  S1 + S2 + S3.

area({circle, _, R}) ->
  math:pi() * R * R;
area({rectangle, _, W, H}) ->
  W * H;
area({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  [S1, S2, S3] = triangle_sides({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}),
  S = (S1 + S2 + S3) / 2,
  math:sqrt(S * (S - S1) * (S - S2) * (S - S3)).

enclose({circle, {X, Y}, R}) ->
  D = R * 2,
  {rectangle, {X - R, Y - R}, D, D};
enclose({rectangle, {X, Y}, W, H}) ->
  {rectangle, {X, Y}, W, H};
enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  Left = minThree(X1, X2, X3),
  Right = maxThree(X1, X2, X3),
  Top = minThree(Y1, Y2, Y3),
  Bottom = maxThree(Y1, Y2, Y3),
  W = (Right - Left),
  H = (Bottom - Top),
  {rectangle, {Left, Top}, W, H}.

triangle_sides({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
  LX1 = X2 - X1, LY1 = Y2 - Y1,
  S1 = math:sqrt(LX1 * LX1 + LY1 * LY1),
  LX2 = X3 - X2, LY2 = Y3 - Y2,
  S2 = math:sqrt(LX2 * LX2 + LY2 * LY2),
  LX3 = X1 - X3, LY3 = Y1 - Y3,
  S3 = math:sqrt(LX3 * LX3 + LY3 * LY3),
  [S1, S2, S3].

minThree(X, Y, Z) ->
  min(min(X, Y), Z).

maxThree(X, Y, Z) ->
  max(max(X, Y), Z).
