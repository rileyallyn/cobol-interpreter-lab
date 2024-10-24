%{
#define YYDEBUG 1
#include <stdio.h>
extern int yylineno;
void yyerror(const char*);
int yylex();
%}

%debug
%define parse.error detailed

%token TOKEN_INT
%token TOKEN_PLUS
%token TOKEN_MINUS
%token TOKEN_MUL
%token TOKEN_DIV
%token TOKEN_LPAREN
%token TOKEN_RPAREN
%token TOKEN_SEMI
%token TOKEN_ERROR

%%
program : expr TOKEN_SEMI;

expr : expr TOKEN_PLUS term
    | expr TOKEN_MINUS term
    | term
    ;

term : term TOKEN_MUL factor
    | term TOKEN_DIV factor
    | factor
    ;

factor: TOKEN_MINUS factor
    | TOKEN_LPAREN expr TOKEN_RPAREN
    | TOKEN_INT
    ;
%%
void yyerror(const char* msg) {
    fprintf(stderr, "Error | Line: %d\n%s\n",yylineno,msg);
}
