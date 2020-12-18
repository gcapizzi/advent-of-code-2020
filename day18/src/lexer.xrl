Definitions.

NUMBER = [0-9]+
WS = [\s\t]
LB = \n|\r\n|\r
OPEN_PAREN = (
CLOSED_PAREN = (

Rules.
[0-9]+      : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
\+          : {token, {plus, TokenLine}}.
\*          : {token, {mult, TokenLine}}.
\(          : {token, {lparen, TokenLine}}.
\)          : {token, {rparen, TokenLine}}.
{WS}        : skip_token.
{LB}        : skip_token.

Erlang code.
