%{
#include "token.h"
%}
DIGIT [0-9]
LETTER [a-zA-Z]
%%
(" "|\t|\n) /* skip whitespace */
IDENTIFICATION { return TOKEN_IDENTIFICATION; }
DIVISION { return TOKEN_KEYWORD_DIVISION; }

%%
int yywrap() { return 1; }
