-module(day08_test).
-compile(export_all).

-include_lib("etest/include/etest.hrl").

-define(LoopingProgram, [{nop, 0},
			 {acc, 1},
			 {jmp, 4},
			 {acc, 3},
			 {jmp, -3},
			 {acc, -99},
			 {acc, 1},
			 {jmp, -4},
			 {acc, 6}]).

-define(TerminatingProgram, [{nop, 0},
			     {acc, 1},
			     {jmp, 4},
			     {acc, 3},
			     {jmp, -3},
			     {acc, -99},
			     {acc, 1},
			     {nop, -4},
			     {acc, 6}]).

test_run_looping_program() ->
	?assert_equal({loop, 5}, day08:run_program(?LoopingProgram)).

test_run_terminating_program() ->
	?assert_equal({terminated, 8}, day08:run_program(?TerminatingProgram)).

test_mutate_to_terminate() ->
	?assert_equal({terminated, 8}, day08:mutate_to_terminate(?LoopingProgram)).
