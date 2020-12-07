-module(day07).

%% API exports
-export([main/1, find_rules_containing/2, count_bags/2]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Rules = maps:from_list(lists:map(fun parse_rule/1, Lines)),
    io:format("Matches: ~p~n", [length(find_rules_containing(Rules, "shiny gold bag"))]),
    io:format("Count: ~p~n", [count_bags(Rules, "shiny gold bag")]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_bags(Rules, Bag) ->
    ContainedBags = maps:get(Bag, Rules, []),
    Counts = lists:map(fun({N, B}) -> N + N * count_bags(Rules, B) end, ContainedBags),
    lists:sum(Counts).

find_rules_containing(Rules, Bag) ->
    Matches = maps:keys(maps:filter(fun(_, Values) -> lists:keymember(Bag, 2, Values) end, Rules)),
    OtherMatches = lists:flatmap(fun(B) -> find_rules_containing(Rules, B) end, Matches),
    lists:usort(Matches ++ OtherMatches).

parse_rule(Line) ->
    [Key, ValuesStr] = string:split(string:trim(Line, trailing, "."), " contain "),
    {normalise(Key), parse_values(ValuesStr)}.

parse_values("no other bags") ->
    [];
parse_values(ValuesStr) ->
    lists:map(fun parse_value/1, string:split(ValuesStr, ", ", all)).

parse_value(ValueStr) ->
    {N, Value} = string:to_integer(ValueStr),
    {N, normalise(Value)}.

normalise(Key) ->
    string:trim(string:trim(Key), trailing, "s").
