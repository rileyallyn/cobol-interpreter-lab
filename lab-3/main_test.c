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

UTEST(scanner, hello) {
  struct token_st tokens[] = {
    {TOKEN_IDENTIFICATION, "IDENTIFICATION"},
    {TOKEN_KEYWORD_DIVISION, "DIVISION"},
    {TOKEN_DOT, "."},
    {TOKEN_PROGRAM_ID, "PROGRAM-ID"},
    {TOKEN_DOT, "."}, 
    {TOKEN_IDENT, "HELLO-WORLD"},
    {TOKEN_DOT, "."},
    {TOKEN_PROCEDURE, "PROCEDURE"},
    {TOKEN_KEYWORD_DIVISION, "DIVISION"},
    {TOKEN_DOT, "."},
    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'Hello World!'"},
    {TOKEN_STOP, "STOP"},
    {TOKEN_RUN, "RUN"},
    {TOKEN_DOT, "."},
    {TOKEN_EOF, ""},
  };

  yyin = fopen("samples/hello-world.cbl", "r");
  ASSERT_TRUE(yyin);
  int index = 0;
  token_t t;
  do {
    printf("index: %d token: %d text: %s\n", index, t, yytext);
    ASSERT_EQ(tokens[index].t, (t = yylex()));
    ASSERT_STREQ(tokens[index].p, yytext);
    ++index;
  } while (t != TOKEN_EOF);
}
