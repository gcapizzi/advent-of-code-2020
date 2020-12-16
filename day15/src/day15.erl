-module(day15).

%% API exports
-export([main/1, play/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Input = lists:nth(1, Args),
    StartingNumbers = lists:map(fun list_to_integer/1, string:split(Input, ",", all)),
    io:format("~p~n", [play(StartingNumbers, 2020)]),
    io:format("~p~n", [play(StartingNumbers, 30000000)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

play(StartingNumbers, Turns) ->
    StartLen = length(StartingNumbers),
    StartLast = lists:last(StartingNumbers),
    NumbersWithTurns = maps:from_list(lists:zip(StartingNumbers, lists:map(fun (N) -> [N] end, lists:seq(1, StartLen)))),
    {Last, _} = lists:foldl(fun(I, {Last, Memory}) ->
                                    LastTurns = maps:get(Last, Memory),
                                    NewLast = case LastTurns of
                                                  [_] -> 0;
                                                  [X, Y|_] -> X - Y
                                              end,
                                    NewMemory = maps:update_with(NewLast, fun(Ts) -> [I|Ts] end, [I], Memory),
                                    {NewLast, NewMemory}
                            end,
                            {StartLast, NumbersWithTurns},
                            lists:seq(StartLen + 1, Turns)),
    Last.
