-module(day06).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    GroupsLines = utils:split_list(Lines, ""),
    CountsAny = lists:map(fun count_answers_any/1, GroupsLines),
    CountsAll = lists:map(fun count_answers_all/1, GroupsLines),
    io:format("Sum of counts (any): ~p~n", [lists:sum(CountsAny)]),
    io:format("Sum of counts (all): ~p~n", [lists:sum(CountsAll)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_answers_any(GroupLines) ->
    Sets = lists:map(fun sets:from_list/1, GroupLines),
    sets:size(sets:union(Sets)).

count_answers_all(GroupLines) ->
    Sets = lists:map(fun sets:from_list/1, GroupLines),
    sets:size(sets:intersection(Sets)).
