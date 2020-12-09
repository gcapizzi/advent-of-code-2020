-module(day08).

%% API exports
-export([main/1, run_program/1, mutate_to_terminate/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Instructions = lists:map(fun parse_inst/1, Lines),
    Acc = run_program(Instructions),
    io:format("~p~n", [Acc]),
    AccMut = mutate_to_terminate(Instructions),
    io:format("~p~n", [AccMut]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_inst(Str) ->
    [InstStr, ArgStr|_] = string:split(Str, " "),
    {list_to_atom(InstStr), list_to_integer(ArgStr)}.

run_program(Instructions) ->
    InstructionsMap = maps:from_list(lists:zip(lists:seq(1, length(Instructions)), Instructions)),
    run_program_map(InstructionsMap).

run_program_map(InstructionsMap) ->
    run_program_map(InstructionsMap, 1, 0, sets:new()).

run_program_map(InstructionsMap, Ic, Acc, Visited) ->
    Looping = sets:is_element(Ic, Visited),
    if
        Looping -> {loop, Acc};
        true ->
            Inst = maps:get(Ic, InstructionsMap, not_found),
            case Inst of
                {acc, N} -> run_program_map(InstructionsMap, Ic + 1, Acc + N, sets:add_element(Ic, Visited));
                {jmp, N} -> run_program_map(InstructionsMap, Ic + N, Acc, sets:add_element(Ic, Visited));
                {nop, _} -> run_program_map(InstructionsMap, Ic + 1, Acc, sets:add_element(Ic, Visited));
                not_found -> {terminated, Acc}
            end
    end.

mutate_to_terminate(Instructions) ->
    InstructionsWithIndex = lists:zip(lists:seq(1, length(Instructions)), Instructions),
    InstructionsMap = maps:from_list(InstructionsWithIndex),
    Mutations = lists:foldr(fun({I, {Inst, N}}, Mutations) ->
                                    case Inst of
                                        jmp -> Mutations ++ [maps:update(I, {nop, N}, InstructionsMap)];
                                        nop -> Mutations ++ [maps:update(I, {jmp, N}, InstructionsMap)];
                                        _Else -> Mutations
                                    end
                            end, [], InstructionsWithIndex),
    Solutions = lists:map(fun run_program_map/1, Mutations),
    lists:keyfind(terminated, 1, Solutions).
