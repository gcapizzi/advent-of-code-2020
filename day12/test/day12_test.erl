-module(day12_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(Instructions, [{forward, 10},
		       {north, 3},
		       {forward, 7},
		       {right, 90},
		       {forward, 11}]).

test_neighbours() ->
	?assert_equal({214, -72}, day12:follow_instructions_2(?Instructions, {0, 0}, {10, 1})).
