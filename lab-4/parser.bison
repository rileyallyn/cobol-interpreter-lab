%{
#define YYDEBUG 1
#include <stdio.h>
extern int yylineno;
void yyerror(const char*);
int yylex();
%}

%debug
%define parse.error detailed

%token TOKEN_ADD
%token TOKEN_ALPHANUMERIC
%token TOKEN_COMPUTATION_LEVEL_0
%token TOKEN_COMPUTATION_LEVEL_1
%token TOKEN_COMPUTATION_LEVEL_2
%token TOKEN_COMPUTATION_LEVEL_3
%token TOKEN_DISPLAY
%token TOKEN_DIVIDE
%token TOKEN_DOT
%token TOKEN_ELSE
%token TOKEN_ELSE_IF
%token TOKEN_END_IF
%token TOKEN_END_PERFORM
%token TOKEN_EQUAL
%token TOKEN_EXPONENTIAL
%token TOKEN_GREATER_THAN
%token TOKEN_IDENT
%token TOKEN_IF
%token TOKEN_IMPLIED_DECIMAL
%token TOKEN_INTEGER
%token TOKEN_KEYWORD_BY
%token TOKEN_KEYWORD_COMPUTE
%token TOKEN_KEYWORD_DATA
%token TOKEN_KEYWORD_DIVISION
%token TOKEN_KEYWORD_FROM
%token TOKEN_KEYWORD_FUNCTION
%token TOKEN_KEYWORD_IDENTIFICATION
%token TOKEN_KEYWORD_OCCURS
%token TOKEN_KEYWORD_SECTION
%token TOKEN_KEYWORD_TO
%token TOKEN_KEYWORD_VALUE
%token TOKEN_LEFT_PARENTHESIS
%token TOKEN_LESS_THAN
%token TOKEN_MOVE
%token TOKEN_MULTIPLY
%token TOKEN_NUMERIC
%token TOKEN_PERFORM
%token TOKEN_PICTURE
%token TOKEN_PROGRAM_ID
%token TOKEN_PROCEDURE
%token TOKEN_RIGHT_PARENTHESIS
%token TOKEN_RUN
%token TOKEN_SIGNED_NUMERIC
%token TOKEN_SPACE
%token TOKEN_STOP
%token TOKEN_STRING
%token TOKEN_SUB
%token TOKEN_UNTIL
%token TOKEN_VARYING
%token TOKEN_WORKING_STORAGE
%token TOKEN_EOF



%%
file            : statements     
statements      : statement_list
                ;
statement_list  : statement_list statement
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
simple_stmt     : cbl_func_stmt
                | if_branch 
                | else_parts
                | perform_stmt
                ;
cbl_func_stmt   : display_stmt
                | move_stmt
                | compute_stmt
                ;

display_stmt    : TOKEN_DISPLAY op_parms_list
                ;
move_stmt       : TOKEN_MOVE op_parms_list assignment_stmt
                ;
compute_stmt    : TOKEN_KEYWORD_COMPUTE single_op_parm assignment_stmt
                ;
assignment_stmt : TOKEN_EQUAL ext_function
                | TOKEN_EQUAL op_parms_list
                | TOKEN_KEYWORD_TO op_parms_list
                ;
op_parms_list   : single_op_parm
                | op_parms_list op_parm
                ;
op_parm         : mathmaticalexpr
                | booleanexpr
                | type_expr
                ;
term            : mathmaticalexpr
                ;
mathmaticalexpr : type_expr
                | container_expr
                | type_expr container_expr
                | mathmaticalexpr TOKEN_ADD term
                | mathmaticalexpr TOKEN_SUB term
                | mathmaticalexpr TOKEN_MULTIPLY term
                | mathmaticalexpr TOKEN_DIVIDE term
                | mathmaticalexpr TOKEN_EXPONENTIAL term
                ;                
container_expr  : TOKEN_LEFT_PARENTHESIS op_parms_list TOKEN_RIGHT_PARENTHESIS
                ;
booleanexpr     : mathmaticalexpr TOKEN_LESS_THAN term
                | mathmaticalexpr TOKEN_GREATER_THAN term
                | mathmaticalexpr TOKEN_EQUAL term
                ;
type_expr       : TOKEN_IDENT
                | TOKEN_INTEGER
                | TOKEN_STRING
                | TOKEN_SPACE
                | TOKEN_SUB TOKEN_IDENT
                ;
ext_function    : TOKEN_KEYWORD_FUNCTION TOKEN_IDENT TOKEN_LEFT_PARENTHESIS TOKEN_IDENT TOKEN_RIGHT_PARENTHESIS
                ;
if_branch       : TOKEN_IF booleanexpr 
                ;
else_parts      : TOKEN_ELSE_IF booleanexpr simple_stmt
                | TOKEN_ELSE simple_stmt
                | TOKEN_END_IF
                ;
perform_stmt    : TOKEN_PERFORM TOKEN_VARYING TOKEN_IDENT TOKEN_KEYWORD_FROM TOKEN_INTEGER TOKEN_KEYWORD_BY TOKEN_INTEGER TOKEN_UNTIL op_parms_list
                | TOKEN_END_PERFORM
                ;
data_space      : TOKEN_WORKING_STORAGE TOKEN_KEYWORD_SECTION TOKEN_DOT
                ;
data_category   : TOKEN_ALPHANUMERIC 
                | TOKEN_NUMERIC 
                | TOKEN_SIGNED_NUMERIC
                | TOKEN_IMPLIED_DECIMAL
                ;
categry_contain : TOKEN_LEFT_PARENTHESIS TOKEN_INTEGER TOKEN_RIGHT_PARENTHESIS
                | TOKEN_LEFT_PARENTHESIS TOKEN_IDENT TOKEN_RIGHT_PARENTHESIS
                ;
complete_category: data_category categry_contain
                | data_category categry_contain complete_category
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
complex_decl    : TOKEN_INTEGER TOKEN_IDENT TOKEN_PICTURE category_spec TOKEN_DOT
                ;
category_spec   : complete_category
                | complete_category data_clauses
                ;
data_clauses    : full_data_clause
                | full_data_clause TOKEN_INTEGER
                ;
data_declaration: simple_decl
                | complex_decl
                ;
single_op_parm  : op_parm
                ;

%%
void yyerror(const char* msg) {
    fprintf(stderr, "Error | Line: %d\n%s\n",yylineno,msg);
}
