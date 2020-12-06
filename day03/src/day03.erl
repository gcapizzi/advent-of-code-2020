-module(day03).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),

    N1 = count_trees(Lines, 1, 1),
    N2 = count_trees(Lines, 3, 1),
    N3 = count_trees(Lines, 5, 1),
    N4 = count_trees(Lines, 7, 1),
    N5 = count_trees(Lines, 1, 2),

    io:format("Trees: ~p~n", [N1 * N2 * N3 * N4 * N5]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_trees(Lines, XInc, YInc) ->
    Xs = lists:seq(1, XInc * length(Lines), XInc),
    Ys = lists:seq(1, length(Lines), YInc),
    Positions = zipmin(Xs, Ys),

    lists:foldl(fun({X, Y}, C) ->
                        Line = lists:nth(Y, Lines),
                        ActualX = ((X - 1) rem length(Line)) + 1,
                        Char = lists:nth(ActualX, Line),
                        if
                            Char =:= $# ->
                                C + 1;
                            true ->
                                C
                        end
                end, 0, Positions).

zipmin(Left, Right) ->
    Len = min(length(Left), length(Right)),
    lists:zip(
      lists:sublist(Left, Len),
      lists:sublist(Right, Len)
     ).

