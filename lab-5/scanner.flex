%{
#include "parser.h"
%}

%option nounput
%option noinput
%option noyywrap
%option yylineno

DIGIT   [0-9]
LETTER  [a-zA-Z_]

%%

[ \n\r\t]*    /* skip whitespace */;
"print"       return TOKEN_KEYWORD_PRINT;
"if"          return TOKEN_KEYWORD_IF;
"then"        return TOKEN_KEYWORD_THEN;
"endif"       return TOKEN_KEYWORD_ENDIF;
{DIGIT}+      return TOKEN_INT;
{LETTER}+     return TOKEN_ID;
","           return TOKEN_COMMA;
"]"           return TOKEN_RBRACKET;
"["           return TOKEN_LBRACKET;
"=="          return TOKEN_EQUAL_EQUAL;
"!="          return TOKEN_NOT_EQUAL;
"<="          return TOKEN_LESS_THAN_OR_EQUAL;
">="          return TOKEN_GREATER_THAN_OR_EQUAL;
"<"           return TOKEN_LESS_THAN;
">"           return TOKEN_GREATER_THAN;
\=            return TOKEN_ASSIGNMENT;
\*            return TOKEN_MUL;
\+            return TOKEN_PLUS;
\-            return TOKEN_MINUS;
\/            return TOKEN_DIV;
\(            return TOKEN_LPAREN;
\)            return TOKEN_RPAREN;
\;            return TOKEN_SEMI;
.             { printf("scan error: bad token: %c\n", yytext[0]); }
%%

/*
flex calls yywrap() whenever it reaches the end of the current file.
If yywrap returns false to indicate the end of the program.
It could alternatively open up another file and return true,
so that flex would keep going.
*/
