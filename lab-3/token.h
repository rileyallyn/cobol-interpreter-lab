typedef enum {
  TOKEN_EOF = 0,
  // Identification Keywords
  TOKEN_IDENTIFICATION,
  TOKEN_KEYWORD_DIVISION,
  TOKEN_KEYWORD_DATA,
  TOKEN_KEYWORD_SECTION,
  TOKEN_PROGRAM_ID,
  TOKEN_WORKING_STORAGE,
  TOKEN_PROCEDURE,

  // Program Keywords
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
  TOKEN_END_IF,
  TOKEN_SPACE,
  TOKEN_KEYWORD_OCCURS,
  TOKEN_KEYWORD_VALUE,

  // Identifiers
  TOKEN_IDENT,
  // Data types
  TOKEN_STRING,
  TOKEN_INTEGER,
  TOKEN_PICTURE,
  TOKEN_ALPHANUMERIC,
  TOKEN_NUMERIC,
  TOKEN_SIGNED_NUMERIC,
  // Grammar
  TOKEN_LEFT_PARENTHESIS,
  TOKEN_RIGHT_PARENTHESIS,
  TOKEN_DOT,
  TOKEN_COMMENT,

  // Operators
  TOKEN_ADD,
  TOKEN_SUB,
  TOKEN_GREATER_THAN,
  TOKEN_LESS_THAN,
} token_t;