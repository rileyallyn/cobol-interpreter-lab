// https://github.com/sheredom/utest.h/blob/master/utest.h
#include "utest.h"
#include <stdio.h>

extern int yyparse();

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern int yyrestart();
extern YY_BUFFER_STATE yy_scan_buffer(char *str, int i);
extern YY_BUFFER_STATE yy_scan_string(char *str);
extern void yy_delete_buffer(YY_BUFFER_STATE buffer);
extern FILE *yyin;
extern int yylineno;

UTEST_MAIN();


UTEST(parser, math) {
  // Must include the null character to terminate input
  char string[] = "1+8/4-3;\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  yylineno = 1;
  int result = yyparse();

  yy_delete_buffer(buffer);

  // Assert the result to test correctness
  ASSERT_EQ(result, 0); 
}

UTEST(parser, missing_semi_colon) {
  // Must include the null character to terminate input
  char string[] = "1+8/4-3\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  yylineno = 1;
  int result = yyparse();

  yy_delete_buffer(buffer);

  // Assert the result to test correctness
  ASSERT_EQ(result, 1); 
}

UTEST(parser, hello) {
  // Read sample file as input
  yyin = fopen("samples/hello-world.cbl", "r");
  yyrestart(yyin);
  ASSERT_TRUE(yyin);

  yylineno = 1;
  int result = yyparse();

  // Assert the result to test correctness
  ASSERT_EQ(result, 0); 
}

UTEST(parser, print) {
  // Must include the null character to terminate input
  char string[] = "DISPLAY 'Hello World!'\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  yylineno = 1;
  int result = yyparse();

  yy_delete_buffer(buffer);

  // Assert the result to test correctness
  ASSERT_EQ(result, 0); 
}

UTEST(parser, branching) {
  // Must include the null character to terminate input
  char string[] = "IF A > B THEN DISPLAY 'A is greater than B' ELSE DISPLAY 'B is greater than A'\0"; 
  YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

  yylineno = 1;
  int result = yyparse();

  yy_delete_buffer(buffer);

  // Assert the result to test correctness
  ASSERT_EQ(result, 0); 
}  