%{
#define YYDEBUG 1
#include <stdio.h>
extern int yylineno;
void yyerror(const char*);
int yylex();
%}

%debug
%define parse.error detailed

%token TOKEN_EOF
%token TOKEN_IDENTIFICATION
%token TOKEN_KEYWORD_DIVISION
%token TOKEN_KEYWORD_DATA
%token TOKEN_KEYWORD_SECTION
%token TOKEN_PROGRAM_ID
%token TOKEN_WORKING_STORAGE
%token TOKEN_PROCEDURE
%token TOKEN_STOP
%token TOKEN_RUN
%token TOKEN_MOVE
%token TOKEN_KEYWORD_TO
%token TOKEN_PERFORM
%token TOKEN_VARYING
%token TOKEN_KEYWORD_FROM
%token TOKEN_KEYWORD_BY
%token TOKEN_UNTIL
%token TOKEN_END_PERFORM
%token TOKEN_IF
%token TOKEN_END_IF
%token TOKEN_SPACE
%token TOKEN_KEYWORD_OCCURS
%token TOKEN_KEYWORD_VALUE
%token TOKEN_KEYWORD_COMPUTE
%token TOKEN_KEYWORD_FUNCTION
%token TOKEN_IDENT
%token TOKEN_STRING
%token TOKEN_INTEGER
%token TOKEN_PICTURE
%token TOKEN_ALPHANUMERIC
%token TOKEN_NUMERIC
%token TOKEN_SIGNED_NUMERIC
%token TOKEN_IMPLIED_DECIMAL
%token TOKEN_COMPUTATION_LEVEL_0
%token TOKEN_COMPUTATION_LEVEL_1
%token TOKEN_COMPUTATION_LEVEL_2
%token TOKEN_COMPUTATION_LEVEL_3
%token TOKEN_LEFT_PARENTHESIS
%token TOKEN_RIGHT_PARENTHESIS
%token TOKEN_DOT
%token TOKEN_COMMENT
%token TOKEN_ADD
%token TOKEN_SUB
%token TOKEN_MULTIPLY
%token TOKEN_DIVIDE
%token TOKEN_EQUAL
%token TOKEN_GREATER_THAN
%token TOKEN_LESS_THAN
%token TOKEN_EXPONENTIAL
%token TOKEN_DISPLAY

%%
file            : statements     
statements      : statements statement
                | statement
                ;
statement       : section
                | sect_data
                | simple_stmt
                ;
section         : type TOKEN_KEYWORD_DIVISION TOKEN_DOT
                | type TOKEN_RUN TOKEN_DOT
                ;
sect_data       : TOKEN_PROGRAM_ID  TOKEN_DOT TOKEN_IDENT TOKEN_DOT
                ;
type            : TOKEN_IDENTIFICATION
                | TOKEN_PROCEDURE
                | TOKEN_STOP
                | TOKEN_KEYWORD_DATA
                ;
simple_stmt     : function
                ;
function        : TOKEN_DISPLAY parms
                ;
parms           : TOKEN_STRING
                ;


%%
void yyerror(const char* msg) {
    fprintf(stderr, "Error | Line: %d\n%s\n",yylineno,msg);
}
