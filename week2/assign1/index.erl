-module(index).
-export([generate/1,get_file_contents/1,show_file_contents/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
  {ok,File} = file:open(Name,[read]),
  Rev = get_all_lines(File,[]),
  lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
  case io:get_line(File,"") of
    eof -> file:close(File),
      Partial;
    Line -> {Strip,_} = lists:split(length(Line)-1,Line),
      get_all_lines(File,[Strip|Partial])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
  io:format("~s~n",[L]),
  show_file_contents(Ls);
show_file_contents([]) ->
  ok.

%%% assignment

% given a file name generates the index for that file
generate(FileName) ->
  normalize_output(words_locations(get_file_contents(FileName))).

% words with less than 3 characters are not valid words
valid_word(Word) when length(Word) < 3 ->
  false;
% top 50 common english words are not valid
valid_word(Word) ->
  not lists:member(Word, ["the", "and", "that", "have", "for",
                          "not", "with", "you", "this", "but",
                          "his", "from", "they", "say", "her",
                          "she", "will", "one", "would", "there",
                          "their", "what", "out", "about", "who",
                          "get", "which"]).

split(Line) -> string:tokens(Line, " :;~`!@#$%^&*()-_=+[{]}'\"\\|,<.>/?").
normalize(Words) -> lists:map(fun string:to_lower/1, Words).
remove_invalid(Words) -> lists:filter(fun valid_word/1, Words).
atomize(Words) -> lists:map(fun list_to_atom/1, Words).

words_locations(Lines) ->
  words_locations(Lines, 1, #{}).
words_locations([], _, Result) ->
  Result;
words_locations([Line|Rest], LineNumber, Result) ->
  Words = atomize(remove_invalid(normalize(split(Line)))),
  words_locations(Rest, LineNumber+1, map_location(Words, LineNumber, Result)).

map_location([], _, Result) ->
  Result;
map_location([Word|Rest], LineNumber, Result) ->
  map_location(Rest, LineNumber, maps:put(Word, [LineNumber | maps:get(Word, Result, [])], Result)).

normalize_output(WordsLocations) ->
  maps:map(fun (_, Locations) -> normalized_locations(Locations) end, WordsLocations).

normalized_locations(Locations) ->
  lists:map(fun (Xs=[X|_]) -> {X, lists:last(Xs)} end, generate_intervals(no_dup(lists:sort(Locations)))).

no_dup([]) -> [];
no_dup([X]) -> [X];
no_dup([X|Xs]) ->
  case lists:member(X, Xs) of
    true  -> no_dup(Xs);
    false -> [X | no_dup(Xs)]
  end.

generate_intervals([]) ->
  [];
generate_intervals(Xs) ->
  lists:reverse(generate_intervals(Xs, [], [])).

generate_intervals([], Prev, Result) ->
  [Prev|Result];
generate_intervals([X|Xs], [], Result) ->
  generate_intervals(Xs, [X], Result);
generate_intervals([X|Xs], Ys=[Y|_], Result) ->
  case X == Y + 1 of
    true  -> generate_intervals(Xs, [X|Ys], Result);
    false -> generate_intervals(Xs, [X], [lists:reverse(Ys)|Result])
  end.
