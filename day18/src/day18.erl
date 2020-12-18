-module(day18).

%% API exports
-export([main/1, eval/1, eval2/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Path = lists:nth(1, Args),
    Lines = utils:file_lines(Path),
    Sum = lists:sum(lists:map(fun eval/1, Lines)),
    io:format("~p~n", [Sum]),
    Sum2 = lists:sum(lists:map(fun eval2/1, Lines)),
    io:format("~p~n", [Sum2]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

eval(Exp) ->
    {ok, Tokens, _} = lexer:string(Exp),
    {ok, Tree} = parser:parse(Tokens),
    eval_tree(Tree).

eval2(Exp) ->
    {ok, Tokens, _} = lexer:string(Exp),
    {ok, Tree} = parser2:parse(Tokens),
    eval_tree(Tree).

eval_tree({number, N}) -> N;
eval_tree({add, X, Y}) -> eval_tree(X) + eval_tree(Y);
eval_tree({mult, X, Y}) -> eval_tree(X) * eval_tree(Y).
