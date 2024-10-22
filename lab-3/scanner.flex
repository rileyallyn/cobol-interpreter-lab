%{
#include "token.h"
%}
DIGIT [0-9]
LETTER [a-zA-Z]
NAME [a-zA-Z0-9][a-zA-Z0-9_-]*[a-zA-Z0-9]

%%
(" "|\t|\n) /* skip whitespace */
\*>\ ?.* { return TOKEN_COMMENT; }
IDENTIFICATION { return TOKEN_IDENTIFICATION; }
DIVISION { return TOKEN_KEYWORD_DIVISION; }
PROGRAM-ID { return TOKEN_PROGRAM_ID; }
PROCEDURE { return TOKEN_PROCEDURE; }
DATA { return TOKEN_KEYWORD_DATA; }
SECTION { return TOKEN_KEYWORD_SECTION; }
WORKING-STORAGE { return TOKEN_WORKING_STORAGE; }
DISPLAY { return TOKEN_DISPLAY; }
STOP { return TOKEN_STOP; }
RUN { return TOKEN_RUN; }
MOVE { return TOKEN_MOVE; }
TO { return TOKEN_KEYWORD_TO; }
VARYING { return TOKEN_VARYING; }
FROM { return TOKEN_KEYWORD_FROM; }
BY { return TOKEN_KEYWORD_BY; }
UNTIL { return TOKEN_UNTIL; }
PERFORM { return TOKEN_PERFORM; }
END-PERFORM { return TOKEN_END_PERFORM; }
IF { return TOKEN_IF; }
END-IF { return TOKEN_END_IF; }
SPACE { return TOKEN_SPACE; }

\+ { return TOKEN_ADD; }
\- { return TOKEN_SUB; }
\> { return TOKEN_GREATER_THAN; }
\< { return TOKEN_LESS_THAN; }

"\""[^"]*"\""   { return TOKEN_STRING; }
"\'"[^']*"\'"   { return TOKEN_STRING; }
"("             { return TOKEN_LEFT_PARENTHESIS; }
")"             { return TOKEN_RIGHT_PARENTHESIS; }


\. { return TOKEN_DOT; }
{NAME} { return TOKEN_IDENT; }
{DIGIT} { return TOKEN_INTEGER; }
%%
int yywrap() { return 1; }
