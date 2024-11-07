typedef enum {
  TOKEN_EOF = 0,
  TOKEN_IDENTIFICATION,
  TOKEN_KEYWORD_DIVISION,
  TOKEN_KEYWORD_DATA,
  TOKEN_KEYWORD_SECTION,
  TOKEN_PROGRAM_ID,
  TOKEN_WORKING_STORAGE,
  TOKEN_PROCEDURE,
  TOKEN_DISPLAY,
  TOKEN_STOP,
  TOKEN_RUN,
  TOKEN_MOVE,
  TOKEN_KEYWORD_TO,
  TOKEN_PERFORM,
  TOKEN_VARYING,
  TOKEN_KEYWORD_FROM,
  TOKEN_KEYWORD_BY,
  TOKEN_UNTIL,
  TOKEN_END_PERFORM,
  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_END_IF,
  TOKEN_SPACE,
  TOKEN_KEYWORD_OCCURS,
  TOKEN_KEYWORD_VALUE,
  TOKEN_KEYWORD_COMPUTE,
  TOKEN_KEYWORD_FUNCTION,
  TOKEN_IDENT,
  TOKEN_STRING,
  TOKEN_INTEGER,
  TOKEN_NEGATIVE_INTEGER,
  TOKEN_PICTURE,
  TOKEN_ALPHANUMERIC,
  TOKEN_NUMERIC,
  TOKEN_SIGNED_NUMERIC,
  TOKEN_IMPLIED_DECIMAL,
  TOKEN_LEVEL_NUM,
  TOKEN_COMPUTATION_LEVEL_0,
  TOKEN_COMPUTATION_LEVEL_1,
  TOKEN_COMPUTATION_LEVEL_2,
  TOKEN_COMPUTATION_LEVEL_3,
  TOKEN_LEFT_PARENTHESIS,
  TOKEN_RIGHT_PARENTHESIS,
  TOKEN_DOT,
  TOKEN_COMMENT,
  TOKEN_ADD,
  TOKEN_SUB,
  TOKEN_MULTIPLY,
  TOKEN_DIVIDE,
  TOKEN_EQUAL,
  TOKEN_GREATER_THAN,
  TOKEN_LESS_THAN,
  TOKEN_EXPONENTIAL,
} token_t;