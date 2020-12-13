-module(day13_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

test_crt() ->
	?assert_equal(23, day13:crt([{2, 3}, {3, 5}, {2, 7}])),
	?assert_equal(1068781, day13:crt([{0, 7}, {-1, 13}, {-4, 59}, {-6, 31}, {-7, 19}])),
	?assert_equal(89469, day13:crt([{6, 11}, {13, 16}, {9, 21}, {19, 25}])).
