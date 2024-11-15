#include "expr.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct SymbolMap *symbol_table = NULL;

/*
Create one node in an expression tree and return the structure.
*/

struct type *type_create(type_t kind, struct type *sub) {
  struct type *t = malloc(sizeof(*t));
  t->kind = kind;
  t->subtype = sub;
  return t;
}

struct decl *decl_create(struct expr *name, struct type *type,
                         struct expr *value, struct stmt *code,
                         struct decl *next) {
  struct decl *d = malloc(sizeof(*d));
  d->name = name;
  d->type = type;
  d->value = value;
  d->code = code;
  d->next = next;
  return d;
}

struct stmt *stmt_create(stmt_t kind, struct decl *decl, struct expr *init_expr,
                         struct expr *expr, struct expr *next_expr,
                         struct stmt *body, struct stmt *else_body,
                         struct stmt *next) {
  struct stmt *s = malloc(sizeof(*s));
  s->kind = kind;
  s->decl = decl;
  s->init_expr = init_expr;
  s->expr = expr;
  s->next_expr = next_expr;
  s->body = body;
  s->else_body = else_body;
  s->next = next;
  return s;
}

struct expr *expr_create(expr_t kind, struct expr *left, struct expr *right) {
  /* Shortcut: sizeof(*e) means "the size of what e points to" */
  struct expr *e = malloc(sizeof(*e));

  e->kind = kind;
  e->left = left;
  e->right = right;
  e->name = "";
  e->integer_value = 0;
  e->float_value = 0.0;
  e->string_literal = "";

  return e;
}

struct expr *expr_create_name(const char *value) {
  struct expr *e = expr_create(EXPR_NAME, 0, 0);
  char *dest = malloc(strlen(value) + 1);
  strcpy(dest, value); // copy contents of source to dest
  e->name = dest;
  return e;
}

struct expr *expr_create_integer_literal(int value) {
  struct expr *e = expr_create(EXPR_INTEGER_LITERAL, 0, 0);
  e->integer_value = value;
  return e;
}

struct expr *expr_create_float_literal(float value) {
  struct expr *e = expr_create(EXPR_FLOAT_LITERAL, 0, 0);
  e->float_value = value;
  return e;
}

struct expr *expr_create_string_literal(const char *value) {
  struct expr *e = expr_create(EXPR_STRING_LITERAL, 0, 0);
  char *dest = malloc(strlen(value) + 1);
  strcpy(dest, value);
  e->string_literal = dest;
  return e;
}

/*
Recursively delete an expression tree.
*/

void expr_delete(struct expr *e) {
  /* Careful: Stop on null pointer. */
  if (!e)
    return;
  expr_delete(e->left);
  expr_delete(e->right);
  free(e);
}

void scope_bind(const char *name, struct expr *value) {
  if (symbol_table == NULL) {
    symbol_table = createSymbolMap();
  }
  insertSymbol(symbol_table, name, value);
}

struct expr *scope_lookup(const char *name) {
  return getSymbol(symbol_table, name);
}

/*
Recursively print an expression tree by performing an
in-order traversal of the tree, printing the current node
between the left and right nodes.
*/

void stmt_print(struct stmt *s) {
  if (!s)
    return;
  

  switch (s->kind) {
  case STMT_DECL:
    decl_print(s->decl);
    printf(";\n");
    break;
  case STMT_EXPR:
    expr_print(s->expr);
    break;
  case STMT_IF:
    printf("if ");
    expr_print(s->expr);
    printf(" then\n");
    stmt_print(s->body);
    printf("endif\n");
    if (!s->else_body) { printf("\n"); break;}
    printf("else ");
    stmt_print(s->else_body);
    break;
  case STMT_PRINT:
    printf("print ");
    expr_print(s->expr);
    printf(";\n");
    break;
  case STMT_BLOCK:
    stmt_print(s->body);
    break;
  // we haven't implemented sections yet
  case STMT_SECTION:
    printf("section\n");
    break;
  case STMT_COMPUTE:
    printf("compute ");
    decl_print(s->decl);
    printf(";\n");
    break;
  case STMT_MOVE:
    printf("move ");
    expr_print(s->decl->name);
    printf(" to ");
    expr_print(s->decl->value);
    printf(";\n");
    break;
  case STMT_END_EXECUTION:
    printf("stop run\n");
    break;
  }

  stmt_print(s->next);
}

void decl_print(struct decl *d) {
  expr_print(d->name);
  printf(" = ");
  expr_print(d->value);
}

void expr_print(struct expr *e) {
  /* Careful: Stop on null pointer. */
  if (!e)
    return;

  const char *close = "";
  if (e->kind == EXPR_ARRAY) {
    printf("[");
    close = "]";
  } else if (e->kind != EXPR_NAME && e->kind != EXPR_SUBSCRIPT &&
             e->kind != EXPR_INTEGER_LITERAL && e->kind != EXPR_FLOAT_LITERAL &&
             e->kind != EXPR_STRING_LITERAL && e->kind != EXPR_CUSTOM_FUNCTION) {
    printf("(");
    close = ")";
  } else if (e->kind == EXPR_CUSTOM_FUNCTION) {
    printf("FUNCTION ");
  }

  expr_print(e->left);

  switch (e->kind) {
  case EXPR_NAME:
    if (e->negative) {
      printf("-");
    }
    printf("%s", e->name);
    break;
  case EXPR_ARRAY:
    if (e->right) {
      printf(", ");
    }
    break;
  case EXPR_SUBSCRIPT:
    printf("[");
    close = "]";
    break;
  case EXPR_ADD:
    printf("+");
    break;
  case EXPR_SUBTRACT:
    printf("-");
    break;
  case EXPR_MULTIPLY:
    printf("*");
    break;
  case EXPR_DIVIDE:
    printf("/");
    break;
  case EXPR_EQUAL_EQUAL:
    printf("==");
    break;
  case EXPR_NOT_EQUAL:
    printf("!=");
    break;
  case EXPR_GREATER_THAN:
    printf(">");
    break;
  case EXPR_GREATER_THAN_OR_EQUAL:
    printf(">=");
    break;
  case EXPR_LESS_THAN:
    printf("<");
    break;
  case EXPR_LESS_THAN_OR_EQUAL:
    printf("<=");
    break;
  case EXPR_INTEGER_LITERAL:
    printf("%d", e->integer_value);
    break;
  case EXPR_FLOAT_LITERAL:
    printf("%f", e->float_value);
    break;
  case EXPR_STRING_LITERAL:
    printf("%s", e->string_literal);
    break;
  case EXPR_EXPONENTIAL:
    printf("**");
    break;
  case EXPR_CUSTOM_FUNCTION:
    printf(" ");
    break;
  }

  expr_print(e->right);

  // Options for closing
  printf("%s", close);
}

/*
Recursively evaluate an expression by performing
the desired operation and returning it up the tree.
*/

void stmt_evaluate(struct stmt *s) {
  if (!s)
    return;

  switch (s->kind) {
  case STMT_DECL:
    decl_evaluate(s->decl);
    break;
  case STMT_EXPR:
    expr_evaluate(s->expr);
    break;
  case STMT_IF:
    if (expr_evaluate(s->expr)) {
      stmt_evaluate(s->body);
    }
    break;
  case STMT_PRINT:
    stmt_evaluate_print(s->expr);
    printf("\n");
    break;
  case STMT_BLOCK:
    stmt_evaluate(s->body);
    break;
  case STMT_SECTION:
    break;
  case STMT_END_EXECUTION:
    break;
  case STMT_COMPUTE:
    stmt_evaluate_compute(s);
    break;
  }

  stmt_evaluate(s->next);
}

void stmt_evaluate_print(struct expr *e) {
  if (!e)
    return;


  if (e->kind == EXPR_STRING_LITERAL) {
      printf("%s", expr_string_evaluate(e));
    } else if (e->kind == EXPR_FLOAT_LITERAL) {
      printf("%f", expr_evaluate(e));
  } else if (e->kind == EXPR_INTEGER_LITERAL ||
               e->kind == EXPR_NAME || e->kind == EXPR_SUBSCRIPT) {
    printf("%.0f", expr_evaluate(e));
  } else {
      printf("runtime error: print expression is not literal\n");
  }

  stmt_evaluate_print(e->right);
}

void stmt_evaluate_compute(struct stmt *s) {
  if (!s)
    return;

  if (s->kind == STMT_COMPUTE) {
    decl_evaluate(s->decl);
  }
}

void decl_evaluate(struct decl *d) {
  if (!d)
    return;

  if (!d->value) {
    printf("decl_evaluate: no value\n");
    return;
  }
  if (d->name->kind == EXPR_NAME && d->value->kind == EXPR_ARRAY) {
    struct expr *e = expr_sub_evaluate(d->value);
    scope_bind(d->name->name, e);
  } else if (d->name->kind == EXPR_NAME) {
    float value = expr_evaluate(d->value);
    struct expr *e = expr_create_float_literal(value);
    scope_bind(d->name->name, e);
  } else if (d->name->kind == EXPR_SUBSCRIPT) {
    float value = expr_evaluate(d->value);
    decl_subscript_evaluate(d->name, value);
  } else {
    printf("Error: expression type unknown for name");
    exit(1);
  }
}

void decl_subscript_evaluate(struct expr *e, float value) {
  /* Careful: Return zero on null pointer. */
  if (!e)
    return;

  if (e->kind != EXPR_SUBSCRIPT) {
    printf("runtime error: not a subscript expression\n");
    exit(1);
  }

  if (e->left->kind != EXPR_NAME) {
    printf("runtime error: subscript has no name\n");
    exit(1);
  }

  // Get array expresion
  struct expr *a = scope_lookup(e->left->name);
  float index = expr_evaluate(e->right);

  // Find the right offset
  while (index > 0) {
    a = a->right;
    index--;
  }
  expr_delete(a->left);
  a->left = expr_create_float_literal(value);
}

const char *expr_string_evaluate(struct expr *e) {
  /* Careful: Return zero on null pointer. */
  if (!e)
    return 0;

  switch (e->kind) {
  case EXPR_NAME:
  case EXPR_ARRAY:
  case EXPR_SUBSCRIPT:
  case EXPR_ADD:
  case EXPR_SUBTRACT:
  case EXPR_MULTIPLY:
  case EXPR_DIVIDE:
  case EXPR_EQUAL_EQUAL:
  case EXPR_NOT_EQUAL:
  case EXPR_GREATER_THAN:
  case EXPR_GREATER_THAN_OR_EQUAL:
  case EXPR_LESS_THAN:
  case EXPR_LESS_THAN_OR_EQUAL:
  case EXPR_INTEGER_LITERAL:
  case EXPR_FLOAT_LITERAL:
    printf("runtime error: not a string expression\n");
    exit(1);
  case EXPR_STRING_LITERAL:
    return e->string_literal;
  }

  return "";
}

struct expr *expr_sub_evaluate(struct expr *e) {
  /* Careful: Return zero on null pointer. */
  if (!e)
    return 0;

  // TODO evaluate each item in the array and save the result

  return e;
}

/*
Recursively evaluate an expression by performing
the desired operation and returning it up the tree.
*/

float expr_subscript_evaluate(struct expr *e) {
  /* Careful: Return zero on null pointer. */
  if (!e)
    return 0;

  if (e->kind != EXPR_SUBSCRIPT) {
    printf("runtime error: not a subscript expression\n");
    exit(1);
  }

  if (e->left->kind != EXPR_NAME) {
    printf("runtime error: subscript has no name\n");
    exit(1);
  }

  // Get array expresion
  struct expr *a = scope_lookup(e->left->name);
  float index = expr_evaluate(e->right);

  // Find the right offset
  while (index > 0) {
    a = a->right;
    index--;
  }
  return expr_evaluate(a->left);
}

float expr_evaluate_custom_function(struct expr *e) {
  if (strcmp(e->left->name, "SQRT") == 0) {
    return sqrt(expr_evaluate(e->right));
  } 

  printf("runtime error: unknown custom function\n");
  exit(1);
}

float expr_evaluate(struct expr *e) {
  /* Careful: Return zero on null pointer. */
  if (!e)
    return 0;

  if (e->kind == EXPR_SUBSCRIPT)
    return expr_subscript_evaluate(e);

  float l = expr_evaluate(e->left);
  float r = expr_evaluate(e->right);
  float result;

  switch (e->kind) {
  case EXPR_NAME:
    // Get the variable expression and then evaluate it.
    if (e->negative) {
      return -expr_evaluate(scope_lookup(e->name));
    }
    return expr_evaluate(scope_lookup(e->name));
  case EXPR_ARRAY:
    printf("runtime error: array in expression\n");
    exit(1);
  case EXPR_SUBSCRIPT:
    printf(
        "runtime error: subscript should be processed earlier in expression\n");
    exit(1);
  case EXPR_ADD:
    return l + r;
  case EXPR_SUBTRACT:
    return l - r;
  case EXPR_MULTIPLY:
    return l * r;
  case EXPR_DIVIDE:
    if (r == 0) {
      printf("runtime error: divide by zero\n");
      exit(1);
    }
    return l / r;
  case EXPR_EQUAL_EQUAL:
    return l == r;
  case EXPR_NOT_EQUAL:
    return l != r;
  case EXPR_GREATER_THAN:
    return l > r;
  case EXPR_GREATER_THAN_OR_EQUAL:
    return l >= r;
  case EXPR_LESS_THAN:
    return l < r;
  case EXPR_LESS_THAN_OR_EQUAL:
    return l <= r;
  case EXPR_INTEGER_LITERAL:
    return e->integer_value;
  case EXPR_FLOAT_LITERAL:
    return e->float_value;
  case EXPR_STRING_LITERAL:
    printf("runtime error: string in expression\n");
    exit(1);
  case EXPR_VALUE:
    return e->integer_value;
  case EXPR_OCCURS:
    return e->integer_value;
  case EXPR_EXPONENTIAL:
    return pow(l, r);
  case EXPR_CUSTOM_FUNCTION:
    result = expr_evaluate_custom_function(e);
    return result;
  }

  return 0;
}


void close_parser() { destroySymbolMap(symbol_table); }
