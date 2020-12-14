-module(day14).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Instructions = lists:map(fun parse_instruction/1, Lines),
    Memory = run_instructions(Instructions, array:new()),
    Sum = lists:sum(array:sparse_to_list(Memory)),
    io:format("Sum: ~p~n", [Sum]),
    Memory2 = run_instructions_v2(Instructions, array:new()),
    Sum2 = lists:sum(array:sparse_to_list(Memory2)),
    io:format("Sum2: ~p~n", [Sum2]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_instruction("mask = " ++ Mask) ->
    {mask, Mask};
parse_instruction("mem[" ++ Rest) ->
    [AddressStr, ValueStr|_] = string:split(Rest, "] = "),
    {mem, list_to_integer(AddressStr), list_to_integer(ValueStr)}.

apply_mask(Mask, Value) ->
    ValueBin = lists:concat(string:pad(integer_to_list(Value, 2), 36, leading, $0)),
    NewValueBin = lists:zipwith(fun(M, V) ->
                                        case M of
                                            $X -> V;
                                            _ -> M
                                        end
                                end, Mask, ValueBin),
    list_to_integer(NewValueBin, 2).

run_instructions(Instructions, Memory) ->
    {_, NewMemory} = lists:foldl(fun(Inst, {Msk, Mem}) ->
                                         case Inst of
                                             {mask, NewMask} -> {NewMask, Mem};
                                             {mem, Address, Value} -> {Msk, array:set(Address, apply_mask(Msk, Value), Mem)}
                                         end
                                 end,
                                 {"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Memory},
                                 Instructions),
    NewMemory.

run_instructions_v2(Instructions, Memory) ->
    {_, NewMemory} = lists:foldl(fun(Inst, {Msk, Mem}) ->
                                         case Inst of
                                             {mask, NewMask} -> {NewMask, Mem};
                                             {mem, Address, Value} ->
                                                 Addresses = apply_mask_v2(Msk, Address),
                                                 {Msk, lists:foldl(fun(A, M) -> 
                                                                           array:set(A, Value, M)
                                                                   end, Mem, Addresses)}
                                         end
                                 end,
                                 {"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", Memory},
                                 Instructions),
    NewMemory.

apply_mask_v2(Mask, Value) ->
    ValueBin = pad_bin(integer_to_list(Value, 2), 36),
    NewValueBin = lists:zipwith(fun(M, V) ->
                                        case M of
                                            $0 -> V;
                                            $1 -> $1;
                                            _ -> $X
                                        end
                                end, Mask, ValueBin),
    NumOfFloating = length(lists:filter(fun(C) -> C =:= $X end, NewValueBin)),
    RangeBin = lists:map(fun(N) -> pad_bin(integer_to_list(N, 2), NumOfFloating) end, lists:seq(0, pow(2, NumOfFloating) - 1)),
    lists:map(fun(B) -> list_to_integer(replaceXs(NewValueBin, B)) end, RangeBin).

replaceXs(StrWithXs, []) -> StrWithXs;
replaceXs(StrWithXs, [C|Cs]) ->
    {Prefix, Rest} = lists:splitwith(fun(C) -> C =/= $X end, StrWithXs),
    Prefix ++ [C] ++ replaceXs(tail(Rest), Cs).

tail([]) -> [];
tail(Xs) -> lists:nthtail(1, Xs).

pad_bin(V, L) -> lists:concat(string:pad(V, L, leading, $0)).

pow(X, Y) -> trunc(math:pow(X, Y)).
