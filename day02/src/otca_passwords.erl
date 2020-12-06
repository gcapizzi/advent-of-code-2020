-module(otca_passwords).

-export([parse/1, validate/1]).

-record(policy, {positions, letter}).
-record(entry, {policy, password}).

parse(Line) ->
    [PolicyStr, Password] = string:split(Line, ": "),
    [PositionsStr, LetterStr] = string:split(PolicyStr, " "),
    Letter = lists:nth(1, LetterStr),
    PositionsStrs = string:split(PositionsStr, "-"),
    Positions = lists:map(fun list_to_integer/1, PositionsStrs),
    #entry{policy=#policy{positions=Positions, letter=Letter}, password=Password}.

validate(Entry) ->
    MatchingPositions = lists:filter(fun(P) -> lists:nth(P, Entry#entry.password) =:= Entry#entry.policy#policy.letter end, Entry#entry.policy#policy.positions),
    length(MatchingPositions) == 1.
