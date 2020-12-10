-module(day10).

%% API exports
-export([main/1, diffs/1, diff_dist/1, split/2, arrangements/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Nums = lists:map(fun list_to_integer/1, utils:file_lines(Path)),
    {Ones, _, Threes} = diff_dist(Nums),
    io:format("# ones x # threes: ~p~n", [Ones * Threes]),
    Arrangements = arrangements(Nums),
    io:format("arrangements: ~p~n", [Arrangements]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

diffs(Adapters) ->
    Sorted = lists:sort(Adapters),
    BuiltIn = lists:last(Sorted) + 3,
    Final = [0] ++ Sorted ++ [BuiltIn],
    Pairs = lists:zip(lists:droplast(Final), lists:nthtail(1, Final)),
    lists:map(fun({X, Y}) -> Y - X end, Pairs).

diff_dist(Adapters) ->
    Diffs = diffs(Adapters),
    lists:foldr(fun(D, {Ones, Twos, Threes}) ->
                        case D of
                            1 -> {Ones + 1, Twos, Threes};
                            2 -> {Ones, Twos + 1, Threes};
                            3 -> {Ones, Twos, Threes + 1};
                            _Else -> {Ones, Twos, Threes}
                        end
                end, {0, 0, 0}, Diffs).

split(_, []) ->
    [];
split(Pred, List) -> 
    {Matching, Rest} = lists:splitwith(Pred, List),
    [Matching] ++ split(fun(X) -> not Pred(X) end, Rest).

% this assumes:
% - differences are always 1 or 3
% - there never are more than 5 consecutive numbers
arrangements(Adapters) ->
    Diffs = diffs(Adapters),
    PartitionedDiffs = split(fun(X) -> X =:= 1 end, Diffs),
    Factors = lists:map(fun(Ds) -> case Ds of
                                       [1, 1] -> 2;
                                       [1, 1, 1] -> 4;
                                       [1, 1, 1, 1] -> 7;
                                       _ -> 1
                                   end
                        end, PartitionedDiffs),
    lists:foldr(fun(X, Y) -> X * Y end, 1, Factors).
