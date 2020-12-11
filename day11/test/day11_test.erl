-module(day11_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(BoardLines, ["L.LL.LL.LL",
		     "LLLLLLL.LL",
		     "L.L.L..L..",
		     "LLLL.LL.LL",
		     "L.LL.LL.LL",
		     "L.LLLLL.LL",
		     "..L.L.....",
		     "LLLLLLLLLL",
		     "L.LLLLLL.L",
		     "L.LLLLL.LL"]).

test_neighbours() ->
	Board = day11:parse_board(?BoardLines),
	?assert_equal([floor, free, free, free, floor, free, free, free], day11:neighbours({1, 1}, Board)),
	?assert_equal([floor, free, free], day11:neighbours({0, 0}, Board)),
	?assert_equal([free, free, floor], day11:neighbours({9, 9}, Board)).  

test_evolution() ->
	Board = day11:parse_board(?BoardLines),
	?assert_equal(37, day11:count_taken(day11:evolve_until_stable(Board))),
	?assert_equal(26, day11:count_taken(day11:evolve_until_stable_2(Board))).

test_visibles() ->
	Board = day11:parse_board([".......#.",
				   "...#.....",
				   ".#.......",
				   ".........",
				   "..#L....#",
				   "....#....",
				   ".........",
				   "#........",
				   "...#....."]),
	?assert_equal([taken, taken, taken, taken, taken, taken, taken, taken], day11:visibles({4, 3}, Board)).
