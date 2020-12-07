-module(day07_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(Rules, #{
	  "light red bag" => [{1, "bright white bag"}, {2, "muted yellow bag"}],
	  "dark orange bag" => [{3, "bright white bag"}, {4, "muted yellow bag"}],
	  "bright white bag" => [{1, "shiny gold bag"}],
	  "muted yellow bag" => [{2, "shiny gold bag"}, {9, "faded blue bag"}],
	  "shiny gold bag" => [{1, "dark olive bag"}, {2, "vibrant plum bag"}],
	  "dark olive bag" => [{3, "faded blue bag"}, {4, "dotted black bag"}],
	  "vibrant plum bag" => [{5, "faded blue bag"}, {6, "dotted black bag"}]
	 }).

test_find_rule_containing() ->
	?assert_equal(["bright white bag", "dark orange bag", "light red bag", "muted yellow bag"], day07:find_rules_containing(?Rules, "shiny gold bag")).

test_count_bags() ->
	?assert_equal(32, day07:count_bags(?Rules, "shiny gold bag")).
