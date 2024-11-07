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
%token TOKEN_KEYWORD_IDENTIFICATION
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
%token TOKEN_ELSE
%token TOKEN_ELSE_IF
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
                | data_space
                | data_declaration
                ;
                
section         : type TOKEN_KEYWORD_DIVISION TOKEN_DOT
                | type TOKEN_RUN TOKEN_DOT
                ;
sect_data       : TOKEN_PROGRAM_ID TOKEN_DOT TOKEN_IDENT TOKEN_DOT
                ;
type            : TOKEN_KEYWORD_IDENTIFICATION
                | TOKEN_PROCEDURE
                | TOKEN_STOP
                | TOKEN_KEYWORD_DATA
                ;
simple_stmt     : cbl_function
                | cbl_function param
                | cbl_function assignment_stmt
                | cbl_function param assignment_stmt
                ;
assignment_stmt : TOKEN_EQUAL ext_function
                | TOKEN_KEYWORD_TO TOKEN_IDENT
                | TOKEN_KEYWORD_TO TOKEN_IDENT categry_contain
                ;
param           : TOKEN_IDENT
                | TOKEN_STRING
                ;
ext_function    : TOKEN_KEYWORD_FUNCTION TOKEN_IDENT TOKEN_LEFT_PARENTHESIS TOKEN_IDENT TOKEN_RIGHT_PARENTHESIS
                ;
cbl_function    : TOKEN_DISPLAY
                | TOKEN_MOVE
                | TOKEN_KEYWORD_COMPUTE
                | TOKEN_PERFORM
                ;
if_branch       : if_branch param if_branch
                | TOKEN_IF
                | TOKEN_ELSE_IF
                | TOKEN_ELSE
                | TOKEN_END_IF
                ;
data_space      : TOKEN_WORKING_STORAGE 
                | TOKEN_KEYWORD_SECTION 
                | TOKEN_DOT
                ;
data_category   : TOKEN_ALPHANUMERIC 
                | TOKEN_NUMERIC 
                | TOKEN_SIGNED_NUMERIC
                | TOKEN_IMPLIED_DECIMAL
                ;
categry_contain : TOKEN_LEFT_PARENTHESIS TOKEN_INTEGER TOKEN_RIGHT_PARENTHESIS
                ;
complete_category: complete_category complete_category  
                | data_category categry_contain
                ;
data_clause     : TOKEN_COMPUTATION_LEVEL_0 
                | TOKEN_COMPUTATION_LEVEL_1 
                | TOKEN_COMPUTATION_LEVEL_2 
                | TOKEN_COMPUTATION_LEVEL_3
                | TOKEN_KEYWORD_VALUE
                | TOKEN_KEYWORD_OCCURS
                ;
full_data_clause: data_clause data_clause
                | data_clause
                ;
simple_decl     : TOKEN_INTEGER TOKEN_IDENT TOKEN_DOT
                ;
full_decl       : TOKEN_INTEGER TOKEN_IDENT TOKEN_PICTURE complete_category TOKEN_DOT
                | TOKEN_INTEGER TOKEN_IDENT TOKEN_PICTURE complete_category full_data_clause TOKEN_DOT
                | TOKEN_INTEGER TOKEN_IDENT TOKEN_PICTURE complete_category full_data_clause TOKEN_INTEGER TOKEN_DOT
                ;
data_declaration: simple_decl
                | full_decl
                ;


%%
void yyerror(const char* msg) {
    fprintf(stderr, "Error | Line: %d\n%s\n",yylineno,msg);
}
