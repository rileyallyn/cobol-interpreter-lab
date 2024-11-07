// https://github.com/sheredom/utest.h/blob/master/utest.h
#include "utest.h"
#include "expr.h"
#include <stdio.h>

/* Clunky: Declare the parse function generated from parser.bison */
extern int yyparse();

/* Clunky: Declare the result of the parser from parser.bison */
extern struct stmt *parser_result;

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern int yyrestart();
extern YY_BUFFER_STATE yy_scan_buffer(char *str, int i);
extern YY_BUFFER_STATE yy_scan_string(char *str);
extern void yy_delete_buffer(YY_BUFFER_STATE buffer);
extern FILE *yyin;
extern int yylineno;

UTEST_MAIN();

void read_file(const char *filename, char *expected_output) {
  // Read the expected output from a file
  FILE *expected_file = fopen(filename, "r");
  if (expected_file == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  size_t n =
      fread(expected_output, 1, sizeof(expected_output) - 1, expected_file);
  expected_output[n] = '\0';
  fclose(expected_file);
}

void redirect_stdout(const char *filename, int evalutate) {
  // Redirect stdout to a temporary file
  FILE *temp_file = fopen(filename, "w");
  if (temp_file == NULL) {
    perror("tmpfile");
    exit(EXIT_FAILURE);
  }
  int temp_fd = fileno(temp_file);
  int stdout_fd = dup(STDOUT_FILENO);
  dup2(temp_fd, STDOUT_FILENO);

  // Perform operations that generate output
  if (evalutate != 0) {
    stmt_evaluate(parser_result);
  } else {
    stmt_print(parser_result);
  }

  // Flush and close stdout
  fflush(stdout);
  dup2(stdout_fd, STDOUT_FILENO);
  close(stdout_fd);
}

UTEST(interpreter, print) {

  yyin = fopen("samples/multiple_statements.c", "r");
  yyrestart(yyin);
  ASSERT_TRUE(yyin);

//   yylineno = 1;
  yylineno = 1;
  int result = yyparse();

  if (result == 0) {
    // Catch the standard output and compare with expected test result
    redirect_stdout("test_print.txt", 0);
    redirect_stdout("test_evaluate.txt", 1);
  }

  // Assert the result to test correctness
  ASSERT_EQ(result, 0);

  char actual_print[1024];
  read_file("test_print.txt", actual_print);
  char expected_print[1024];
  read_file("samples/multiple_statements_print.txt", expected_print);
  ASSERT_STREQ(actual_print, expected_print);

  char actual_evaluate[1024];
  read_file("test_evaluate.txt", actual_evaluate);
  char expected_evaluate[1024];
  read_file("samples/multiple_statements_evaluate.txt", expected_evaluate);
  ASSERT_STREQ(actual_evaluate, expected_evaluate);
}



UTEST(interpreter, program) {
  yyin = fopen("samples/program.c", "r");
  yyrestart(yyin);
  ASSERT_TRUE(yyin);

  yylineno = 1;
  int result = yyparse();

  if (result == 0) {
    // Catch the standard output and compare with expected test result
    redirect_stdout("test_print.txt", 0);
    redirect_stdout("test_evaluate.txt", 1);
  }

  // Assert the result to test correctness
  ASSERT_EQ(result, 0);

  char actual_print[1024];
  read_file("test_print.txt", actual_print);
  char expected_print[1024];
  read_file("samples/program_print.txt", expected_print);
  ASSERT_STREQ(actual_print, expected_print);

  char actual_evaluate[1024];
  read_file("test_evaluate.txt", actual_evaluate);
  char expected_evaluate[1024];
  read_file("samples/program_evaluate.txt", expected_evaluate);
  ASSERT_STREQ(actual_evaluate, expected_evaluate);
}

// UTEST(parser, missing_new_line) {
//   // Must include the null character to terminate input
//   char string[] = "1+8/4-3\0";
//   YY_BUFFER_STATE buffer = yy_scan_buffer(string, sizeof(string));

//   yylineno = 1;
//   int result = yyparse();

//   yy_delete_buffer(buffer);

//   // Assert the result to test correctness
//   ASSERT_EQ(result, 1);
// }

// UTEST(parser, hello_world) {
//   // Read sample file as input
//   yyin = fopen("samples/hello.py", "r");
//   yyrestart(yyin);
//   ASSERT_TRUE(yyin);

//   yylineno = 1;
//   int result = yyparse();

//   // Assert the result to test correctness
//   ASSERT_EQ(result, 0);
// }

// UTEST(parser, quadratic) {
//   // Read sample file as input
//   yyin = fopen("samples/quadratic.py", "r");
//   yyrestart(yyin);
//   ASSERT_TRUE(yyin);

//   yylineno = 1;
//   int result = yyparse();

//   // Assert the result to test correctness
//   ASSERT_EQ(result, 0);
// }
