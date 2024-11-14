%{
#include "token.h"
%}
%option warn
%option nodefault
%option yylineno

NAME [a-zA-Z]([a-zA-Z0-9_-]*[a-zA-Z0-9])?
DIGIT [0-9]+
%%

(" "|\t|\n) /* skip whitespace */
\*>\ ?.*
IDENTIFICATION { return TOKEN_KEYWORD_IDENTIFICATION; }
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
ELSE { return TOKEN_ELSE; }
END-IF { return TOKEN_END_IF; }
SPACE { return TOKEN_SPACE; }
PIC { return TOKEN_PICTURE; }
OCCURS { return TOKEN_KEYWORD_OCCURS; }
VALUE { return TOKEN_KEYWORD_VALUE; }
COMPUTE { return TOKEN_KEYWORD_COMPUTE; }
FUNCTION { return TOKEN_KEYWORD_FUNCTION; }
X { return TOKEN_ALPHANUMERIC; }
S9 { return TOKEN_SIGNED_NUMERIC; }
9 { return TOKEN_NUMERIC; }
V9 { return TOKEN_IMPLIED_DECIMAL; }
COMP { return TOKEN_COMPUTATION_LEVEL_0; }
COMP-1 { return TOKEN_COMPUTATION_LEVEL_1; }
COMP-2 { return TOKEN_COMPUTATION_LEVEL_2; }
COMP-3 { return TOKEN_COMPUTATION_LEVEL_3; }


\+ { return TOKEN_ADD; }
\- { return TOKEN_SUB; }
\*\* { return TOKEN_EXPONENTIAL; }
\* { return TOKEN_MULTIPLY; }
\/ { return TOKEN_DIVIDE; }
\> { return TOKEN_GREATER_THAN; }
\< { return TOKEN_LESS_THAN; }
\= { return TOKEN_EQUAL;}

"\""[^"]*"\""   { return TOKEN_STRING; }
"\'"[^']*"\'"   { return TOKEN_STRING; }
"("             { return TOKEN_LEFT_PARENTHESIS; }
")"             { return TOKEN_RIGHT_PARENTHESIS; }
{NAME} { return TOKEN_IDENT; }
{DIGIT} { return TOKEN_INTEGER; }

\. { return TOKEN_DOT; }
%%
int yywrap() { return 1; }
