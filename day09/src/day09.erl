-module(day09).

%% API exports
-export([main/1, find_sum_pair/2, find_invalid_num/2, find_sum_sublist/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Nums = lists:map(fun list_to_integer/1, utils:file_lines(Path)),
    {value, InvalidNum} = find_invalid_num(Nums, 25),
    io:format("Invalid value: ~p~n", [InvalidNum]),
    {value, Sublist} = find_sum_sublist(InvalidNum, Nums),
    EncryptionWeakness = lists:min(Sublist) + lists:max(Sublist),
    io:format("Encryption weakness: ~p~n", [EncryptionWeakness]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_sum_pair(Sum, Numbers) ->
    Pairs = pairs(Numbers),
    lists:search(fun({X, Y}) -> X + Y =:= Sum end, Pairs).

pairs(List) ->
    [{X, Y} || X <- List, Y <- List, X < Y].

find_invalid_num(Numbers, PreambleSize) ->
    Is = lists:seq(1, length(Numbers) - PreambleSize - 1),
    Res = lists:search(fun(I) ->
                        Preamble = lists:sublist(Numbers, I, PreambleSize),
                        N = lists:nth(I + PreambleSize, Numbers),
                        Pair = find_sum_pair(N, Preamble),
                        case Pair of
                            {value, _} -> false;
                            _ -> true
                        end
                end, Is),
    case Res of
        {value, N} -> {value, lists:nth(N + PreambleSize, Numbers)};
        _ -> false
    end.

find_sum_sublist(Sum, Numbers) ->
    Ranges = pairs(lists:seq(1, length(Numbers))),
    Sublists = lists:map(fun({From, To}) -> lists:sublist(Numbers, From, To - From + 1) end, Ranges),
    lists:search(fun(Sublist) -> lists:sum(Sublist) =:= Sum end, Sublists).
