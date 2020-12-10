-module(day09_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(Numbers, [35,
		  20,
		  15,
		  25,
		  47,
		  40,
		  62,
		  55,
		  65,
		  95,
		  102,
		  117,
		  150,
		  182,
		  127,
		  219,
		  299,
		  277,
		  309,
		  576
		 ]).

test_find_sum_pair() ->
	?assert_equal({value, {1, 25}}, day09:find_sum_pair(26, lists:seq(1, 25))),
	?assert_equal({value, {24, 25}}, day09:find_sum_pair(49, lists:seq(1, 25))),
	?assert_equal(false, day09:find_sum_pair(100, lists:seq(1, 25))),
	?assert_equal(false, day09:find_sum_pair(50, lists:seq(1, 25))).

test_find_invalid_number() ->
	?assert_equal({value, 127}, day09:find_invalid_num(?Numbers, 5)).

test_find_sum_sublist() ->
	?assert_equal({value, [15, 25, 47, 40]}, day09:find_sum_sublist(127, ?Numbers)).
