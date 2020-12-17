-module(day16_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(Rules, [{"a", [{1, 3}, {5, 7}]},
		{"b", [{6, 11}, {33, 44}]},
		{"c", [{13, 40}, {45, 50}]}]).

test_validate_ticket() ->
	?assert_equal([], day16:validate_ticket([7, 3, 47], ?Rules)),
	?assert_equal([4], day16:validate_ticket([40, 4, 50], ?Rules)),
	?assert_equal([55], day16:validate_ticket([55, 2, 20], ?Rules)),
	?assert_equal([12], day16:validate_ticket([38, 6, 12], ?Rules)).
