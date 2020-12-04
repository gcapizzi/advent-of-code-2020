-module(day04).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = lines(Path),
    PassportLines = lists:map(fun(Ls) -> joinwith(" ", Ls) end, splitlist(Lines, "")),
    Passports = lists:map(fun passports:parse/1, PassportLines),
    ValidPassports = lists:filter(fun passports:validate/1, Passports),
    io:format("Valid passports: ~p~n", [length(ValidPassports)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

lines(Path) ->
    {ok, Data} = file:read_file(Path),
    lists:map(fun binary_to_list/1, string:split(string:trim(Data), "\n", all)).

splitlist([], _) ->
    [];
splitlist(List, Separator) ->
    {H, TWithSep} = lists:splitwith(fun(X) -> X =/= Separator end, List),
    T = lists:dropwhile(fun(X) -> X =:= Separator end, TWithSep),
    [H|splitlist(T, Separator)].

joinwith(Sep, List) -> lists:append(lists:join(Sep, List)).
