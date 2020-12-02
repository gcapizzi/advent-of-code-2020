-module(srp_passwords).

-export([parse/1, validate/1]).

-record(policy, {range, letter}).
-record(entry, {policy, password}).

parse(Line) ->
    [PolicyStr, Password] = string:split(Line, ": "),
    [RangeStr, LetterStr] = string:split(PolicyStr, " "),
    Letter = lists:nth(1, binary_to_list(LetterStr)),
    [RangeMinStr, RangeMaxStr] = string:split(RangeStr, "-"),
    Range = {binstr_to_int(RangeMinStr), binstr_to_int(RangeMaxStr)},
    #entry{policy=#policy{range=Range, letter=Letter}, password=binary_to_list(Password)}.

validate(Entry) ->
    Matches = lists:filter(fun(C) -> C =:= Entry#entry.policy#policy.letter end, Entry#entry.password),
    in_range(Entry#entry.policy#policy.range, length(Matches)).

binstr_to_int(BinStr) ->
    list_to_integer(binary_to_list(BinStr)).

in_range({Min, Max}, N) ->
    (N >= Min) and (N =< Max).
