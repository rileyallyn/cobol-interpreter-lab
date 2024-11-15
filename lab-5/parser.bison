%{
#define YYDEBUG 1
#include <stdio.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "expr.h"

/*
YYSTYPE is the lexical value returned by each rule in a bison grammar.
By default, it is an integer. In this example, we are returning a pointer to an expression.
*/

// #define YYSTYPE struct stmt *

/*
Clunky: Manually declare the interface to the scanner generated by flex. 
*/

extern char *yytext;
extern int yylex();
void yyerror(const char*);

extern int yylineno;

extern struct stmt *parser_result = 0;

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
%token TOKEN_ELSE_IF
%token TOKEN_ELSE
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
%token TOKEN_ADD
%token TOKEN_SUB
%token TOKEN_MULTIPLY
%token TOKEN_DIVIDE
%token TOKEN_EQUAL
%token TOKEN_GREATER_THAN
%token TOKEN_LESS_THAN
%token TOKEN_EXPONENTIAL
%token TOKEN_DISPLAY


%left TOKEN_ADD TOKEN_SUB
%left TOKEN_MULTIPLY TOKEN_DIVIDE
%left TOKEN_EXPONENTIAL

%union {
    struct expr *expr;
    struct stmt *stmt;
    struct decl *decl;
    struct type *type;
}

%type <stmt> statement_list statement section stop_run sect_data simple_stmt cbl_func_stmt cbl_function if_branch else_parts else_branch perform_stmt data_space
%type <expr> mathmaticalexpr booleanexpr term op_parm container_expr type_expr op_parms math_op categry_contain category_value ident ext_function
%type <decl> assignment_stmt simple_decl complex_decl data_declaration category_spec
%type <type> data_category complete_category data_clause
%%
file            : statement_list
                    {parser_result = $1; return 0;}
                ;
statement_list  : statement statement_list
                    { $$ = $1; $1->next = $2;}
                | statement
                    { $$ = $1; }
                ;
statement       : section
                    {$$ = $1;}
                | sect_data
                    {$$ = $1;}
                | simple_stmt
                    {$$ = stmt_create(STMT_BLOCK, NULL, NULL, NULL, NULL, $1, NULL, NULL);}
                | data_space
                    {$$ = stmt_create(STMT_SECTION, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                | data_declaration
                    {$$ = stmt_create(STMT_DECL, $1, NULL, NULL, NULL, NULL, NULL, NULL);}
                | stop_run
                    {$$ = $1;}
                ;
section         : type TOKEN_KEYWORD_DIVISION TOKEN_DOT
                    {$$ = stmt_create(STMT_SECTION, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                ;
stop_run        : TOKEN_STOP TOKEN_RUN TOKEN_DOT
                    {$$ = stmt_create(STMT_END_EXECUTION, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                ;
sect_data       : TOKEN_PROGRAM_ID TOKEN_DOT ident TOKEN_DOT
                    {$$ = stmt_create(STMT_SECTION, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                ;
type            : TOKEN_KEYWORD_IDENTIFICATION
                | TOKEN_PROCEDURE
                | TOKEN_KEYWORD_DATA
                ;
simple_stmt     : cbl_func_stmt
                    {$$ = $1;}
                | if_branch
                | perform_stmt
                ;
cbl_func_stmt   : cbl_function ident assignment_stmt
                    { $3->name = $2; $$= stmt_create($1->kind, $3, NULL, NULL, NULL, NULL, NULL, NULL);}
                | cbl_function op_parms
                    {$$ = stmt_create($1->kind, NULL, NULL, $2, NULL, NULL, NULL, NULL);}
                ;
assignment_stmt : TOKEN_EQUAL op_parms
                    {$$ = decl_create(NULL, NULL, $2, NULL, NULL);}
                | TOKEN_KEYWORD_TO op_parms
                    {$$ = decl_create(NULL, NULL, $2, NULL, NULL);}
                ;
op_parms        : op_parm
                    {$$ = $1;}
                | op_parm op_parms
                    {$$ = $1; $1->right = $2;}
                ;
op_parm         : mathmaticalexpr
                    {$$ = $1;}
                | booleanexpr
                    {$$ = $1;}
                ;
term            : mathmaticalexpr
                    {$$ = $1;}
                ;
math_op         : TOKEN_ADD
                    {$$ = expr_create(EXPR_ADD, NULL, NULL);}
                | TOKEN_SUB
                    {$$ = expr_create(EXPR_SUBTRACT, NULL, NULL);}
                | TOKEN_MULTIPLY
                    {$$ = expr_create(EXPR_MULTIPLY, NULL, NULL);}
                | TOKEN_DIVIDE
                    {$$ = expr_create(EXPR_DIVIDE, NULL, NULL);}
                | TOKEN_EXPONENTIAL
                    {$$ = expr_create(EXPR_EXPONENTIAL, NULL, NULL);}
                ;
mathmaticalexpr : type_expr
                    {$$ = $1;}
                | mathmaticalexpr math_op term
                    {$$ = expr_create($2->kind, $1, $3);}
                | container_expr
                    {$$ = $1;}
                | type_expr container_expr
                    {$$ = $1; $1->right = $2;}
                ;
container_expr  : TOKEN_LEFT_PARENTHESIS mathmaticalexpr TOKEN_RIGHT_PARENTHESIS
                    {$$ = $2;}
                ;
booleanexpr     : mathmaticalexpr TOKEN_LESS_THAN term
                    {$$ = expr_create(EXPR_LESS_THAN, $1, $3);}
                | mathmaticalexpr TOKEN_GREATER_THAN term
                    {$$ = expr_create(EXPR_GREATER_THAN, $1, $3);}
                | mathmaticalexpr TOKEN_EQUAL term
                    {$$ = expr_create(EXPR_EQUAL_EQUAL, $1, $3);}
                ;
type_expr       : ident
                    {$$ = expr_create_name(yytext);}
                | TOKEN_INTEGER
                    {$$ = expr_create_integer_literal(atoi(yytext));}
                | TOKEN_STRING
                    {$$ = expr_create_string_literal(yytext);}
                | TOKEN_SPACE
                    {$$ = expr_create_integer_literal(0);}
                // TODO: implment negative numbers
                | TOKEN_SUB ident
                    { $2->negative = 1; $$ = $2;}
                | ext_function
                    {$$ = $1;}
                ;
ext_function    : TOKEN_KEYWORD_FUNCTION ident TOKEN_LEFT_PARENTHESIS ident TOKEN_RIGHT_PARENTHESIS
                    {$$ = expr_create(EXPR_CUSTOM_FUNCTION, $2, $4);}
                ;
cbl_function    : TOKEN_DISPLAY
                    {$$ = stmt_create(STMT_PRINT, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                | TOKEN_MOVE
                    {$$ = stmt_create(STMT_MOVE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                | TOKEN_KEYWORD_COMPUTE
                    {$$ = stmt_create(STMT_COMPUTE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);}
                ;
if_branch       : TOKEN_IF booleanexpr statement_list else_parts
                    {$$ = stmt_create(STMT_IF, NULL, NULL, $2, NULL, $3, $4, NULL);}
                ;
else_parts      : else_branch
                    {$$ = $1;}
                | else_parts else_branch
                    {$$ = $1; $1->else_body = $2;}
else_branch     : TOKEN_ELSE_IF booleanexpr simple_stmt
                    {$$ = stmt_create(STMT_IF, NULL, NULL, $2, NULL, $3, NULL, NULL);}
                | TOKEN_ELSE simple_stmt
                    {$$ = stmt_create(STMT_IF, NULL, NULL, NULL, NULL, $2, NULL, NULL);}
                | TOKEN_END_IF
                    {$$ = NULL;}
perform_stmt    : TOKEN_PERFORM TOKEN_VARYING ident TOKEN_KEYWORD_FROM TOKEN_INTEGER TOKEN_KEYWORD_BY TOKEN_INTEGER TOKEN_UNTIL op_parms
                | TOKEN_END_PERFORM
                ;
data_space      : TOKEN_WORKING_STORAGE TOKEN_KEYWORD_SECTION TOKEN_DOT
                ;
data_category   : TOKEN_ALPHANUMERIC
                    {$$ = type_create(TYPE_ALPHANUMERIC, NULL);}
                | TOKEN_NUMERIC 
                    {$$ = type_create(TYPE_NUMERIC, NULL);}
                | TOKEN_SIGNED_NUMERIC
                    {$$ = type_create(TYPE_SIGNED_NUMERIC, NULL);}
                | TOKEN_IMPLIED_DECIMAL
                    {$$ = type_create(TYPE_IMPLIED_DECIMAL, NULL);}
                ;
categry_contain : TOKEN_LEFT_PARENTHESIS TOKEN_INTEGER TOKEN_RIGHT_PARENTHESIS
                    {$$ = expr_create_integer_literal(atoi(yytext));}
                ;
complete_category: data_category categry_contain
                    {$$ = $1; $1->limit = $2->integer_value;}
                | data_category categry_contain complete_category
                    {$1->limit = $2->integer_value; $1->subtype = $3; $$ = $1;}
                ;
data_clause     : TOKEN_COMPUTATION_LEVEL_0
                    {$$ = type_create(TYPE_ALPHANUMERIC, NULL); $$->level = LEVEL_0;}
                | TOKEN_COMPUTATION_LEVEL_1 
                    {$$ = type_create(TYPE_ALPHANUMERIC, NULL); $$->level = LEVEL_1;}
                | TOKEN_COMPUTATION_LEVEL_2 
                    {$$ = type_create(TYPE_ALPHANUMERIC, NULL); $$->level = LEVEL_2;}
                | TOKEN_COMPUTATION_LEVEL_3
                    {$$ = type_create(TYPE_ALPHANUMERIC, NULL); $$->level = LEVEL_3;}
                |
                ;
category_value  : TOKEN_KEYWORD_VALUE TOKEN_INTEGER
                    {$$ = expr_create_integer_literal(atoi(yytext)); $$->kind = EXPR_VALUE;}
                | TOKEN_KEYWORD_OCCURS TOKEN_INTEGER
                   {$$ = expr_create_integer_literal(atoi(yytext)); $$->kind = EXPR_OCCURS;}
                |
                   {$$ = expr_create(EXPR_NULL, NULL, NULL);}
                ;
category_spec   : complete_category
                    {$$ = decl_create(NULL, $1, NULL, NULL, NULL);}
                | complete_category data_clause category_value
                    { $$ = decl_create(NULL, $1, $3, NULL, $2);}
                ;
                //TODO: implement levels
simple_decl     : TOKEN_INTEGER ident TOKEN_DOT
                    {$$ = decl_create($2, NULL, expr_create(EXPR_NULL, NULL, NULL), NULL, NULL);}
                ;
complex_decl    : TOKEN_INTEGER ident TOKEN_PICTURE category_spec TOKEN_DOT
                    {$$ = decl_create($2, $4->type, $4->value, NULL, NULL);}
                ;


data_declaration: simple_decl
                    {$$ = $1;}
                | complex_decl
                    {$$ = $1;}
                ;
ident           : TOKEN_IDENT
                { $$ = expr_create_name(yytext);}
                ;
%%
void yyerror(const char* msg) {
    fprintf(stderr, "Error | Line: %d\n%s\n",yylineno,msg);
}