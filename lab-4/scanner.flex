%{
#include "token.h"
%}

%option nounput
%option noinput
%option yylineno

DIGIT [0-9]
LETTER [a-zA-Z]

%%
(" "|\t|\n) /* skip whitespace */
\+ { return TOKEN_PLUS; }
\- { return TOKEN_MINUS; }
\* { return TOKEN_MUL; }
\/ { return TOKEN_DIV; }
\( { return TOKEN_LPAREN; }
\) { return TOKEN_RPAREN; }
\; { return TOKEN_SEMI; }
{DIGIT}+ { return TOKEN_INT; }
. { return TOKEN_ERROR; }
%%

int yywrap() { return 1; }
