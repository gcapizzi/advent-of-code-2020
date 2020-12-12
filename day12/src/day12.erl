-module(day12).

%% API exports
-export([main/1, follow_instructions_2/3]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Instructions = lists:map(fun parse_instruction/1, Lines),
    Position = follow_instructions(Instructions, {0, 0}, east),
    io:format("~p~n", [Position]),
    Position2 = follow_instructions_2(Instructions, {0, 0}, {10, 1}),
    io:format("~p~n", [Position2]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_instruction([Action|Value]) ->
    {parse_action(Action), list_to_integer(Value)}.

parse_action($N) -> north;
parse_action($S) -> south;
parse_action($E) -> east;
parse_action($W) -> west;
parse_action($L) -> left;
parse_action($R) -> right;
parse_action($F) -> forward.

follow_instructions(Instructions, Position, Direction) ->
    {NewPosition, _} = lists:foldl(fun({A, V}, {{X, Y}, D}) ->
                                           case A of
                                               north -> {{X, Y - V}, D};
                                               south -> {{X, Y + V}, D};
                                               east -> {{X + V, Y}, D};
                                               west -> {{X - V, Y}, D};
                                               left -> {{X, Y}, rotate_left(V, D)};
                                               right -> {{X, Y}, rotate_right(V, D)};
                                               forward -> {move_forward({X, Y}, V, D), D}
                                           end
                                   end, {Position, Direction}, Instructions),
    NewPosition.

rotate_left(90, north) -> west;
rotate_left(180, north) -> south;
rotate_left(270, north) -> east;
rotate_left(90, east) -> north;
rotate_left(180, east) -> west;
rotate_left(270, east) -> south;
rotate_left(90, south) -> east;
rotate_left(180, south) -> north;
rotate_left(270, south) -> west;
rotate_left(90, west) -> south;
rotate_left(180, west) -> east;
rotate_left(270, west) -> north.

rotate_right(90, north) -> east;
rotate_right(180, north) -> south;
rotate_right(270, north) -> west;
rotate_right(90, east) -> south;
rotate_right(180, east) -> west;
rotate_right(270, east) -> north;
rotate_right(90, south) -> west;
rotate_right(180, south) -> north;
rotate_right(270, south) -> east;
rotate_right(90, west) -> north;
rotate_right(180, west) -> east;
rotate_right(270, west) -> south.

move_forward({X, Y}, V, north) -> {X, Y - V};
move_forward({X, Y}, V, east) -> {X + V, Y};
move_forward({X, Y}, V, south) -> {X, Y + V};
move_forward({X, Y}, V, west) -> {X - V, Y}.

follow_instructions_2(Instructions, Position, WaypointPosition) ->
    {NewPosition, _} = lists:foldl(fun({A, V}, {Pos, {WX, WY}}) ->
                                           case A of
                                               north -> {Pos, {WX, WY + V}};
                                               south -> {Pos, {WX, WY - V}};
                                               east -> {Pos, {WX + V, WY}};
                                               west -> {Pos, {WX - V, WY}};
                                               left -> {Pos, rotate_vector_left(V, {WX, WY})};
                                               right -> {Pos, rotate_vector_right(V, {WX, WY})};
                                               forward -> {apply_vector({WX, WY}, Pos, V), {WX, WY}}
                                           end
                                   end, {Position, WaypointPosition}, Instructions),
    NewPosition.

rotate_vector_left(90, {X, Y}) -> {-Y, X};
rotate_vector_left(180, {X, Y}) -> {-X, -Y};
rotate_vector_left(270, {X, Y}) -> {Y, -X}.

rotate_vector_right(90, Pos) -> rotate_vector_left(270, Pos);
rotate_vector_right(180, Pos) -> rotate_vector_left(180, Pos);
rotate_vector_right(270, Pos) -> rotate_vector_left(90, Pos).

apply_vector({VX, VY}, {X, Y}, V) -> {X + V * VX, Y + V * VY}.
