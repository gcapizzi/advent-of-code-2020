-module(day01).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Nums = lists:map(fun binstr_to_int/1, lines(Path)),

    Pairs = pairs(Nums),
    {value, {X1, Y1}} = lists:search(fun({X, Y}) -> X + Y =:= 2020 end, Pairs),
    io:format("~p * ~p = ~p~n", [X1, Y1, X1 * Y1]),

    Triples = triples(Nums),
    {value, {X2, Y2, Z2}} = lists:search(fun({X, Y, Z}) -> X + Y + Z =:= 2020 end, Triples),
    io:format("~p * ~p * ~p = ~p~n", [X2, Y2, Z2, X2 * Y2 * Z2]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

lines(Path) ->
    {ok, Data} = file:read_file(Path),
    string:split(string:trim(Data), "\n", all).

pairs(List) ->
    [{X, Y} || X <- List, Y <- List, X < Y].

triples(List) ->
    [{X, Y, Z} || X <- List, Y <- List, Z <- List, X < Y, Y < Z].

binstr_to_int(BinStr) ->
    list_to_integer(binary_to_list(BinStr)).
