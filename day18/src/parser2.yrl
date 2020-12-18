Nonterminals
expression term factor.

Terminals
number plus mult lparen rparen.

Rootsymbol expression.

expression -> factor: '$1'.
expression -> expression mult factor: { mult, '$1', '$3' }.

factor -> factor plus term: { add, '$1', '$3' }.
factor -> term: '$1'.

term -> number: { number, unwrap('$1') }.
term -> lparen expression rparen: '$2'.

Erlang code.

unwrap({_,_,V}) -> V.
