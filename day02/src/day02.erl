-module(day02).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = lines(Path),

    SRPEntries = lists:map(fun srp_passwords:parse/1, Lines),
    ValidSRPEntries = lists:filter(fun srp_passwords:validate/1, SRPEntries),
    io:format("SRP: ~p~n", [length(ValidSRPEntries)]),

    OTCAEntries = lists:map(fun otca_passwords:parse/1, Lines),
    ValidOTCAEntries = lists:filter(fun otca_passwords:validate/1, OTCAEntries),
    io:format("OTCA: ~p~n", [length(ValidOTCAEntries)]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

lines(Path) ->
    {ok, Data} = file:read_file(Path),
    string:split(string:trim(Data), "\n", all).
