-module(day13).

%% API exports
-export([main/1, crt/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    [StartTimeStr, BusesStr|_] = utils:file_lines(Path),
    StartTime = list_to_integer(StartTimeStr),
    BusStrs = string:split(BusesStr, ",", all),
    BusStrsWithIndexes = lists:zip(lists:seq(0, 1 - length(BusStrs), -1), BusStrs),
    BusesWithIndexes = lists:map(fun({I, B}) -> {I, list_to_integer(B)} end, lists:filter(fun({_, C}) -> C =/= "x" end, BusStrsWithIndexes)),
    io:format("~p~n", [BusesWithIndexes]),
    BusesWithWaitTimes = lists:map(fun({_, B}) -> {B, B - (StartTime rem B)} end, BusesWithIndexes),
    First = lists:nth(1, lists:keysort(2, BusesWithWaitTimes)),
    io:format("~p~n", [First]),
    N = crt(BusesWithIndexes),
    io:format("~p~n", [N]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

crt(AsAndMs) ->
    {_, Ms} = lists:unzip(AsAndMs),
    M = lists:foldl(fun(Mi, M) -> Mi * M end, 1, Ms),
    Res = lists:foldr(fun({Ai, Mi}, Res) ->
                        Zi = M div Mi,
                        Yi = mod_inv(Zi, Mi),
                        Wi = mod((Yi * Zi), M),
                        Res + Ai * Wi
                end, 0, AsAndMs),
    mod(Res, M).

mod_inv(N, Mod) ->
    {ModInv, _} = egcd(N, Mod),
    if
        ModInv < 0 -> ModInv + Mod;
        true -> ModInv
    end.

egcd(_, 0) -> {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - (A div B)*T}.

mod(A, M) ->
    X = A rem M,
    if
        X < 0 -> X + M;
        true -> X
    end.
