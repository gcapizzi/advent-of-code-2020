-module(utils).

-export([file_lines/1, split_list/2]).

file_lines(Path) ->
    {ok, Data} = file:read_file(Path),
    lists:map(fun binary_to_list/1, string:split(string:trim(Data), "\n", all)).

split_list([], _) ->
    [];
split_list(List, Separator) ->
    {H, TWithSep} = lists:splitwith(fun(X) -> X =/= Separator end, List),
    T = lists:dropwhile(fun(X) -> X =:= Separator end, TWithSep),
    [H|split_list(T, Separator)].
