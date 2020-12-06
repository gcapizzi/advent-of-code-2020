-module(day04).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    PassportLines = lists:map(fun(Ls) -> joinwith(" ", Ls) end, utils:split_list(Lines, "")),
    Passports = lists:map(fun passports:parse/1, PassportLines),
    ValidPassports = lists:filter(fun passports:validate/1, Passports),
    io:format("Valid passports: ~p~n", [length(ValidPassports)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

joinwith(Sep, List) -> lists:append(lists:join(Sep, List)).
