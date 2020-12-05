-module(day05_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_find_seat() ->
	?assert_equal({44, 5}, day05:find_seat("FBFBBFFRLR")),
	?assert_equal({70, 7}, day05:find_seat("BFFFBBFRRR")), 
	?assert_equal({14, 7}, day05:find_seat("FFFBBBFRRR")), 
	?assert_equal({102, 4}, day05:find_seat("BBFFBBFRLL")).

test_seat_id() ->
	?assert_equal(357, day05:seat_id("FBFBBFFRLR")),
	?assert_equal(567, day05:seat_id("BFFFBBFRRR")), 
	?assert_equal(119, day05:seat_id("FFFBBBFRRR")), 
	?assert_equal(820, day05:seat_id("BBFFBBFRLL")).

test_find_hole() ->
	?assert_equal(42, day05:find_hole([45, 41, 43, 44, 40])).
