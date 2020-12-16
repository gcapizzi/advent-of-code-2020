-module(day15_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_play() ->
	?assert_equal(436, day15:play([0, 3, 6], 2020)),
	?assert_equal(1, day15:play([1, 3, 2], 2020)),
	?assert_equal(10, day15:play([2, 1, 3], 2020)),
	?assert_equal(27, day15:play([1, 2, 3], 2020)),
	?assert_equal(78, day15:play([2, 3, 1], 2020)),
	?assert_equal(438, day15:play([3, 2, 1], 2020)),
	?assert_equal(1836, day15:play([3, 1, 2], 2020)).
	% ?assert_equal(175594, day15:play([0, 3, 6], 30000000)),
	% ?assert_equal(2578, day15:play([1, 3, 2], 30000000)),
	% ?assert_equal(3544142, day15:play([2, 1, 3], 30000000)),
	% ?assert_equal(261214, day15:play([1, 2, 3], 30000000)),
	% ?assert_equal(6895259, day15:play([2, 3, 1], 30000000)),
	% ?assert_equal(18, day15:play([3, 2, 1], 30000000)),
	% ?assert_equal(362, day15:play([3, 1, 2], 30000000)).
