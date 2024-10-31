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
    // printf("index: %d token: %d text: %s\n", index, t, yytext);
    ASSERT_EQ(tokens[index].t, (t = yylex()));
    ASSERT_STREQ(tokens[index].p, yytext);
    ++index;
  } while (t != TOKEN_EOF);
}

UTEST(scanner, quadratic) {
  struct token_st tokens[] = {
    {TOKEN_IDENTIFICATION, "IDENTIFICATION"},
    {TOKEN_KEYWORD_DIVISION, "DIVISION"},
    {TOKEN_DOT, "."},
    {TOKEN_PROGRAM_ID, "PROGRAM-ID"},
    {TOKEN_DOT, "."}, 
    {TOKEN_IDENT, "QuadraticSolver"},
    {TOKEN_DOT, "."},
    {TOKEN_KEYWORD_DATA, "DATA"},
    {TOKEN_KEYWORD_DIVISION, "DIVISION"},
    {TOKEN_DOT, "."},

    {TOKEN_WORKING_STORAGE, "WORKING_STORAGE,"},
    {TOKEN_KEYWORD_SECTION, "SECTION"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "a"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_KEYWORD_VALUE, "VALUE"},
    {TOKEN_INTEGER, "1"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "b"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_KEYWORD_VALUE, "VALUE"},
    {TOKEN_INTEGER, "5"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "c"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_KEYWORD_VALUE, "VALUE"},
    {TOKEN_INTEGER, "6"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "discriminant"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "root1"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "root2"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_DOT, "."},

    {TOKEN_LEVEL_NUM, "77"},
    {TOKEN_IDENT, "square-root-discriminant"},
    {TOKEN_PICTURE, "PIC"},
    {TOKEN_NUMERIC, "S9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_NUMERIC, "V9"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "5"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_COMPUTATION_LEVEL_3, "COMP-3"},
    {TOKEN_DOT, "."},

    {TOKEN_EOF, ""},

    {TOKEN_PROCEDURE, "PROCEDURE"},
    {TOKEN_KEYWORD_DIVISION, "DIVISION"},
    {TOKEN_DOT, "."},
    {TOKEN_COMMENT, "*> program begins here"},
    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'EQUATION: (1x^2) + 5x + 6 = 0'"},
    {TOKEN_COMPUTE, "COMPUTE"},
    {TOKEN_IDENT, "discriminant"},
    {TOKEN_EQUAL, "="},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_IDENT, "b"},
    {TOKEN_EXPONENTIAL, "**"},
    {TOKEN_INTEGER, "2"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_SUB, "-"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "4"},
    {TOKEN_MULTIPLY, "*"},
    {TOKEN_IDENT, "a"},
    {TOKEN_MULTIPLY, "*"},
    {TOKEN_IDENT, "c"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},

    {TOKEN_EOF, ""},

    {TOKEN_IF, "IF"},
    {TOKEN_IDENT, "discriminant"},
    {TOKEN_GREATER_THAN, ">"},
    {TOKEN_INTEGER, "0"},

    {TOKEN_COMPUTE, "COMPUTE"},
    {TOKEN_IDENT, "square-root-discriminant"},
    {TOKEN_EQUAL, "="},
    {TOKEN_KEYWORD_FUNCTION, "FUNCTION"},
    {TOKEN_IDENT, "SQRT"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_IDENT, "discriminant"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},


    {TOKEN_COMPUTE, "COMPUTE"},
    {TOKEN_IDENT, "root1"},
    {TOKEN_EQUAL, "="},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_SIGNED_NUMERIC, "-b"},
    {TOKEN_ADD, "+"},
    {TOKEN_IDENT, "square-root-discriminant"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_DIVIDE, "/"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "2"},
    {TOKEN_MULTIPLY, "*"},
    {TOKEN_IDENT, "a"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},

    {TOKEN_COMPUTE, "COMPUTE"},
    {TOKEN_IDENT, "root2"},
    {TOKEN_EQUAL, "="},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_SIGNED_NUMERIC, "-b"},
    {TOKEN_SUB, "-"},
    {TOKEN_IDENT, "square-root-discriminant"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},
    {TOKEN_DIVIDE, "/"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "2"},
    {TOKEN_MULTIPLY, "*"},
    {TOKEN_IDENT, "a"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},

    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'The equation has two distinct real roots: '"},

    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'Root 1: '"},
    {TOKEN_IDENT, "root1"},

    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'Root 2: '"},
    {TOKEN_IDENT, "root2"},

    {TOKEN_EOF, ""},

    {TOKEN_ELSE, "ELSE"},
    {TOKEN_IF, "IF"},
    {TOKEN_IDENT, "discriminant"},
    {TOKEN_EQUAL, "="},
    {TOKEN_INTEGER, "0"},

    {TOKEN_COMPUTE, "COMPUTE"},
    {TOKEN_IDENT, "root1"},
    {TOKEN_EQUAL, "="},
    {TOKEN_SIGNED_NUMERIC, "-b"},
    {TOKEN_DIVIDE, "/"},
    {TOKEN_LEFT_PARENTHESIS, "("},
    {TOKEN_INTEGER, "2"},
    {TOKEN_MULTIPLY, "*"},
    {TOKEN_IDENT, "a"},
    {TOKEN_RIGHT_PARENTHESIS, ")"},


    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'The equation has one real root: '"},

    {TOKEN_DISPLAY, "DISPLAY"},   
    {TOKEN_STRING, "Root:"},
    {TOKEN_IDENT, "root1"},


    {TOKEN_ELSE, "ELSE"},

    {TOKEN_DISPLAY, "DISPLAY"},
    {TOKEN_STRING, "'The equation has no real roots.'"},

    {TOKEN_EOF, ""},

    {TOKEN_STOP, "STOP"},
    {TOKEN_RUN, "RUN"},
    {TOKEN_DOT, "."},
    {TOKEN_EOF, ""},
  };

  yyin = fopen("samples/quadratic-snippet.cbl", "r");
  ASSERT_TRUE(yyin);
  int index = 0;
  token_t t;
  do {
    // printf("index: %d token: %d text: %s\n", index, t, yytext);
    ASSERT_EQ(tokens[index].t, (t = yylex()));
    ASSERT_STREQ(tokens[index].p, yytext);
    ++index;
  } while (t != TOKEN_EOF);
}