-module(day10_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(SmallBag, [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]).
-define(LargeBag, [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]).

test_diffs() ->
	?assert_equal([1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3], day10:diffs(?SmallBag)).  

test_diff_dist() ->
	?assert_equal({22, 0, 10}, day10:diff_dist(?LargeBag)).

test_arrangements() ->
	?assert_equal(8, day10:arrangements(?SmallBag)),
	?assert_equal(19208, day10:arrangements(?LargeBag)).

test_split() ->
	?assert_equal([[1, 1, 1], [3, 3], [1], [3]], day10:split(fun(X) -> X =:= 1 end, [1, 1, 1, 3, 3, 1, 3])).
