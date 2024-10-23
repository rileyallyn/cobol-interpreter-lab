#include "token.h"
// https://github.com/sheredom/utest.h/blob/master/utest.h
#include "utest.h"
#include <stdio.h>

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern void yyrestart(FILE * input_file);
extern YY_BUFFER_STATE yy_scan_buffer(char *str, int i);
extern void yy_delete_buffer(YY_BUFFER_STATE buffer);
extern FILE *yyin;
extern int yylex();
extern char *yytext;

UTEST_MAIN();

struct token_st {
  token_t t;
  char *p;
};

UTEST(scanner, identifier) {
  token_t t;
  // Must include the null character to terminate input
  char string[] = "test\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  ASSERT_EQ(TOKEN_IDENT, (t = yylex()));
  ASSERT_STREQ("test", yytext);

  ASSERT_EQ(TOKEN_EOF, (t = yylex()));
  ASSERT_STREQ("", yytext);

  yy_delete_buffer(buffer);
}

UTEST(scanner, assignment) {
  token_t t;
  // Must include the null character to terminate input
  char string[] = "=\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  ASSERT_EQ(TOKEN_ASSIGNMENT, (t = yylex()));
  ASSERT_STREQ("=", yytext);

  ASSERT_EQ(TOKEN_EOF, (t = yylex()));
  ASSERT_STREQ("", yytext);

  yy_delete_buffer(buffer);
}

UTEST(scanner, hello) {
  struct token_st tokens[] = {
    {TOKEN_IDENTIFICATION, "IDENTIFICATION"},
    {TOKEN_PROGRAM_ID, "PROGRAM-ID. HELLO-WORLD."}
    {TOKEN_PROCEDURE_DIVISION, "PROCEDURE DIVISION."},
    {TOKEN_STRING, "Hello World!"},
    {TOKEN_KEYWORD_PRINT, "DISPLAY"},
    {TOKEN_EOF, "STOP RUN."},
  };

UTEST(scanner, sample) {
  struct token_st tokens[] = {
    {TOKEN_IDENT, "answer"},
    {TOKEN_ASSIGNMENT, "="},
    {TOKEN_NUMBER, "2020"},
    {TOKEN_ADD, "+"},
    {TOKEN_NUMBER, "4"},
    {TOKEN_EOF, ""}
  };

  yyin = fopen("samples/program.c", "r");
  yyrestart(yyin);
  ASSERT_TRUE(yyin);

  int index = 0;
  token_t t;
  do {
    ASSERT_EQ(tokens[index].t, (t = yylex()));
    ASSERT_STREQ(tokens[index].p, yytext);
    ++index;
  } while (t != TOKEN_EOF);
}
