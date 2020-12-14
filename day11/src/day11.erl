-module(day11).

%% API exports
-export([main/1, parse_board/1, neighbours/2, evolve_until_stable/1, count_taken/1, visibles/2, evolve_until_stable_2/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Board = parse_board(Lines),
    io:format("#1: ~p~n", [count_taken(evolve_until_stable(Board))]),
    io:format("#2: ~p~n", [count_taken(evolve_until_stable_2(Board))]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_board(Lines) ->
    array:from_list(lists:map(fun parse_board_row/1, Lines)).

parse_board_row(Row) ->
    array:from_list(lists:map(fun parse_board_cell/1, Row)).

parse_board_cell($L) -> free;
parse_board_cell($#) -> taken;
parse_board_cell(_) -> floor.

show_board(Board) ->
    Lines = array:map(fun(_, Row) -> show_board_row(Row) end, Board),
    array:foldr(fun(_, X, Y) -> X ++ "\n" ++ Y end, "", Lines).

show_board_row(Row) ->
    array:to_list(array:map(fun(_, Cell) -> show_board_cell(Cell) end, Row)).

show_board_cell(free) -> $L;
show_board_cell(taken) -> $#;
show_board_cell(_) -> $..

neighbours({Row, Col}, Board) ->
    Cells = [get_cell({Row - 1, Col}, Board),
             get_cell({Row - 1, Col + 1}, Board),
             get_cell({Row, Col + 1}, Board),
             get_cell({Row + 1, Col + 1}, Board),
             get_cell({Row + 1, Col}, Board),
             get_cell({Row + 1, Col - 1}, Board),
             get_cell({Row, Col - 1}, Board),
             get_cell({Row - 1, Col - 1}, Board)],
    reject_undefined(Cells).

get_cell({RowN, ColN}, _) when ((RowN < 0) or (ColN < 0)) -> undefined;
get_cell({RowN, ColN}, Board) ->
    Row = array:get(RowN, Board),
    if
        Row =/= undefined -> array:get(ColN, Row);
        true -> undefined
    end.

reject_undefined(List) ->
    lists:filter(fun(X) -> X =/= undefined end, List).

evolve_board(Board) ->
    array:map(fun(RowN, Row) ->
                      array:map(fun(ColN, Cell) ->
                                        TakenNeighbours = taken_neighbours({RowN, ColN}, Board),
                                        if
                                            (Cell =:= free) and (TakenNeighbours =:= 0) -> taken;
                                            (Cell =:= taken) and (TakenNeighbours >= 4) -> free;
                                            true -> Cell
                                        end
                                end, Row)
              end, Board).

taken_neighbours(Coord, Board) ->
    length(lists:filter(fun(C) -> C =:= taken end, neighbours(Coord, Board))).

evolve_until_stable(Board) ->
    NewBoard = evolve_board(Board),
    if
        NewBoard =:= Board -> Board;
        true -> evolve_until_stable(NewBoard)
    end.

count_taken(Board) ->
    array:foldr(fun(_, Row, Count) ->
                        Count + array:foldr(fun(_, Cell, RowCount) ->
                                                    RowCount + case Cell of
                                                                   taken -> 1;
                                                                   _Else -> 0
                                                               end
                                            end, 0, Row)
                end, 0, Board).

visibles(Coord, Board) ->
    Cells = lists:map(fun(Dir) ->
                              Line = line(Dir, Coord, Board),
                              LineCells = lists:map(fun(C) -> get_cell(C, Board) end, Line),
                              search(fun(C) -> (C =:= free) or (C =:= taken) end, LineCells)
                      end, [n, ne, e, se, s, sw, w, nw]),
    reject_undefined(Cells).

line(n, {RowN, ColN}, _) -> zipmin(lists:seq(RowN - 1, 0, -1), repeat(RowN, ColN));
line(s, {RowN, ColN}, Board) ->
    Rows = array:size(Board),
    zipmin(lists:seq(RowN + 1, Rows - 1), repeat(Rows - RowN - 1, ColN));
line(w, {RowN, ColN}, _) -> zipmin(repeat(ColN, RowN), lists:seq(ColN - 1, 0, -1));
line(e, {RowN, ColN}, Board) ->
    Row = array:get(RowN, Board),
    Cols = array:size(Row),
    zipmin(repeat(Cols - ColN - 1, RowN), lists:seq(ColN + 1, Cols - 1));
line(ne, {RowN, ColN}, Board) ->
    Row = array:get(RowN, Board),
    Cols = array:size(Row),
    zipmin(lists:seq(RowN - 1, 0, -1), lists:seq(ColN + 1, Cols - 1));
line(nw, {RowN, ColN}, _) -> zipmin(lists:seq(RowN - 1, 0, -1), lists:seq(ColN - 1, 0, -1));
line(se, {RowN, ColN}, Board) ->
    Rows = array:size(Board),
    Row = array:get(RowN, Board),
    Cols = array:size(Row),
    zipmin(lists:seq(RowN + 1, Rows - 1), lists:seq(ColN + 1, Cols - 1));
line(sw, {RowN, ColN}, Board) ->
    Rows = array:size(Board),
    zipmin(lists:seq(RowN + 1, Rows - 1), lists:seq(ColN - 1, 0, -1)).

zipmin(Left, Right) ->
    Len = min(length(Left), length(Right)),
    lists:zip(
      lists:sublist(Left, Len),
      lists:sublist(Right, Len)
     ).

repeat(Times, _) when Times < 0 -> [];
repeat(Times, Val) -> lists:duplicate(Times, Val).

search(Pred, List) ->
    case lists:search(Pred, List) of
        {value, V} -> V;
        _Else -> undefined
    end.

evolve_board_2(Board) ->
    array:map(fun(RowN, Row) ->
                      array:map(fun(ColN, Cell) ->
                                        TakenVisibles = taken_visibles({RowN, ColN}, Board),
                                        if
                                            (Cell =:= free) and (TakenVisibles =:= 0) -> taken;
                                            (Cell =:= taken) and (TakenVisibles >= 5) -> free;
                                            true -> Cell
                                        end
                                end, Row)
              end, Board).

taken_visibles(Coord, Board) ->
    length(lists:filter(fun(C) -> C =:= taken end, visibles(Coord, Board))).

evolve_until_stable_2(Board) ->
    NewBoard = evolve_board_2(Board),
    if
        NewBoard =:= Board -> Board;
        true -> evolve_until_stable_2(NewBoard)
    end.
