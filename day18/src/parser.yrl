Nonterminals
expression term factor.

Terminals
number plus mult lparen rparen.

Rootsymbol expression.

expression -> term: '$1'.
expression -> expression plus term: { add, '$1', '$3' }.
expression -> expression mult term: { mult, '$1', '$3' }.

term -> number: { number, unwrap('$1') }.
term -> lparen expression rparen: '$2'.

Erlang code.

unwrap({_,_,V}) -> V.
