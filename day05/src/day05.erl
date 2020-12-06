-module(day05).

%% API exports
-export([main/1, find_seat/1, seat_id/1, find_hole/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    SeatIds = lists:map(fun seat_id/1, Lines),
    io:format("Highest Seat ID: ~p~n", [lists:max(SeatIds)]),
    io:format("Your Seat ID: ~p~n", [find_hole(SeatIds)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_seat(Str) ->
    {{Row, _}, {Col, _}} = lists:foldl(fun(Char, {{RowMin, RowMax}, {ColMin, ColMax}}) ->
                                               case Char of
                                                   $F -> {{RowMin, RowMin + ((RowMax - RowMin) div 2)}, {ColMin, ColMax}};
                                                   $B -> {{RowMin + ((RowMax - RowMin) div 2) + 1, RowMax}, {ColMin, ColMax}};
                                                   $L -> {{RowMin, RowMax}, {ColMin, ColMin + ((ColMax - ColMin) div 2)}};
                                                   $R -> {{RowMin, RowMax}, {ColMin + ((ColMax - ColMin) div 2) + 1, ColMax}}
                                               end
                                       end, {{0, 127}, {0, 7}}, Str),
    {Row, Col}.

seat_id(Str) ->
    {Row, Col} = find_seat(Str),
    Row * 8 + Col.

find_hole(Nums) ->
    SortedNums = lists:sort(Nums),
    Pairs = lists:zip(lists:droplast(SortedNums), lists:nthtail(1, SortedNums)),
    {value, {Min, _}} = lists:search(fun({X, Y}) -> Y - X =/= 1 end, Pairs),
    Min + 1.
