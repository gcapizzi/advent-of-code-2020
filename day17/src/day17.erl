-module(day17).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    InitialState = parse_initial_state(Lines, 0),
    FinalState = iterate(fun evolve_state/1, InitialState, 6),
    io:format("~p~n", [sets:size(FinalState)]),
    InitialState4D = parse_initial_state_4d(Lines, 0, 0),
    FinalState4D = iterate(fun evolve_state_4d/1, InitialState4D, 6),
    io:format("~p~n", [sets:size(FinalState4D)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_initial_state(Lines, Z) ->
    LinesWithIndex = lists:zip(lists:seq(0, length(Lines) - 1), Lines),
    lists:foldr(fun({Y, Line}, State) -> sets:union(State, parse_initial_state_row(Line, Y, Z)) end, sets:new(), LinesWithIndex).

parse_initial_state_row(Row, Y, Z) ->
    RowWithIndex = lists:zip(lists:seq(0, length(Row) - 1), Row),
    lists:foldr(fun({X, Cell}, State) ->
                        case Cell of
                            $# -> sets:add_element({X, Y, Z}, State);
                            _Else -> State
                        end
                end, sets:new(), RowWithIndex).

evolve_state(State) ->
    Inactives = sets:fold(fun(C, Is) ->
                                  InactiveNeighbours = sets:subtract(neighbours(C), State),
                                  sets:union(Is, InactiveNeighbours)
                          end, sets:new(), State),
    SurvivedActives = sets:filter(fun(C) ->
                                          NumOfActiveNeighbours = sets:size(sets:intersection(neighbours(C), State)),
                                          (NumOfActiveNeighbours =:= 2) or (NumOfActiveNeighbours =:= 3)
                                  end, State),
    NewActives = sets:filter(fun(C) ->
                                     NumOfActiveNeighbours = sets:size(sets:intersection(neighbours(C), State)),
                                     NumOfActiveNeighbours =:= 3
                             end, Inactives),
    sets:union(SurvivedActives, NewActives).

neighbours({X, Y, Z}) ->
    sets:from_list([{X + DX, Y + DY, Z + DZ} || DX <- [-1, 0, 1], DY <- [-1, 0, 1], DZ <- [-1, 0, 1], [DX, DY, DZ] =/= [0, 0, 0]]).

parse_initial_state_4d(Lines, Z, W) ->
    LinesWithIndex = lists:zip(lists:seq(0, length(Lines) - 1), Lines),
    lists:foldr(fun({Y, Line}, State) -> sets:union(State, parse_initial_state_row_4d(Line, Y, Z, W)) end, sets:new(), LinesWithIndex).

parse_initial_state_row_4d(Row, Y, Z, W) ->
    RowWithIndex = lists:zip(lists:seq(0, length(Row) - 1), Row),
    lists:foldr(fun({X, Cell}, State) ->
                        case Cell of
                            $# -> sets:add_element({X, Y, Z, W}, State);
                            _Else -> State
                        end
                end, sets:new(), RowWithIndex).

evolve_state_4d(State) ->
    Inactives = sets:fold(fun(C, Is) ->
                                  InactiveNeighbours = sets:subtract(neighbours_4d(C), State),
                                  sets:union(Is, InactiveNeighbours)
                          end, sets:new(), State),
    SurvivedActives = sets:filter(fun(C) ->
                                          NumOfActiveNeighbours = sets:size(sets:intersection(neighbours_4d(C), State)),
                                          (NumOfActiveNeighbours =:= 2) or (NumOfActiveNeighbours =:= 3)
                                  end, State),
    NewActives = sets:filter(fun(C) ->
                                     NumOfActiveNeighbours = sets:size(sets:intersection(neighbours_4d(C), State)),
                                     NumOfActiveNeighbours =:= 3
                             end, Inactives),
    sets:union(SurvivedActives, NewActives).

neighbours_4d({X, Y, Z, W}) ->
    sets:from_list([{X + DX, Y + DY, Z + DZ, W + DW} || DX <- [-1, 0, 1], DY <- [-1, 0, 1], DZ <- [-1, 0, 1], DW <- [-1, 0, 1], [DX, DY, DZ, DW] =/= [0, 0, 0, 0]]).

iterate(Fn, Init, 1) -> Fn(Init);
iterate(Fn, Init, N) -> Fn(iterate(Fn, Init, N - 1)).
