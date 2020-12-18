-module(day18_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_eval() ->
	?assert_equal(71, day18:eval("1 + 2 * 3 + 4 * 5 + 6")),
	?assert_equal(51, day18:eval("1 + (2 * 3) + (4 * (5 + 6))")),
	?assert_equal(26, day18:eval("2 * 3 + (4 * 5)")),
	?assert_equal(437, day18:eval("5 + (8 * 3 + 9 + 3 * 4 * 3)")),
	?assert_equal(12240, day18:eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
	?assert_equal(13632, day18:eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).

test_eval2() ->
	?assert_equal(231, day18:eval2("1 + 2 * 3 + 4 * 5 + 6")),
	?assert_equal(51, day18:eval2("1 + (2 * 3) + (4 * (5 + 6))")),
	?assert_equal(46, day18:eval2("2 * 3 + (4 * 5)")),
	?assert_equal(1445, day18:eval2("5 + (8 * 3 + 9 + 3 * 4 * 3)")),
	?assert_equal(669060, day18:eval2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")),
	?assert_equal(23340, day18:eval2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")).
