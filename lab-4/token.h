/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_TOKEN_H_INCLUDED
# define YY_YY_TOKEN_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    TOKEN_EOF = 258,               /* TOKEN_EOF  */
    TOKEN_IDENTIFICATION = 259,    /* TOKEN_IDENTIFICATION  */
    TOKEN_KEYWORD_DIVISION = 260,  /* TOKEN_KEYWORD_DIVISION  */
    TOKEN_KEYWORD_DATA = 261,      /* TOKEN_KEYWORD_DATA  */
    TOKEN_KEYWORD_SECTION = 262,   /* TOKEN_KEYWORD_SECTION  */
    TOKEN_PROGRAM_ID = 263,        /* TOKEN_PROGRAM_ID  */
    TOKEN_WORKING_STORAGE = 264,   /* TOKEN_WORKING_STORAGE  */
    TOKEN_PROCEDURE = 265,         /* TOKEN_PROCEDURE  */
    TOKEN_STOP = 266,              /* TOKEN_STOP  */
    TOKEN_RUN = 267,               /* TOKEN_RUN  */
    TOKEN_MOVE = 268,              /* TOKEN_MOVE  */
    TOKEN_KEYWORD_TO = 269,        /* TOKEN_KEYWORD_TO  */
    TOKEN_PERFORM = 270,           /* TOKEN_PERFORM  */
    TOKEN_VARYING = 271,           /* TOKEN_VARYING  */
    TOKEN_KEYWORD_FROM = 272,      /* TOKEN_KEYWORD_FROM  */
    TOKEN_KEYWORD_BY = 273,        /* TOKEN_KEYWORD_BY  */
    TOKEN_UNTIL = 274,             /* TOKEN_UNTIL  */
    TOKEN_END_PERFORM = 275,       /* TOKEN_END_PERFORM  */
    TOKEN_IF = 276,                /* TOKEN_IF  */
    TOKEN_ELSE = 277,              /* TOKEN_ELSE  */
    TOKEN_ELSE_IF = 278,           /* TOKEN_ELSE_IF  */
    TOKEN_END_IF = 279,            /* TOKEN_END_IF  */
    TOKEN_SPACE = 280,             /* TOKEN_SPACE  */
    TOKEN_KEYWORD_OCCURS = 281,    /* TOKEN_KEYWORD_OCCURS  */
    TOKEN_KEYWORD_VALUE = 282,     /* TOKEN_KEYWORD_VALUE  */
    TOKEN_KEYWORD_COMPUTE = 283,   /* TOKEN_KEYWORD_COMPUTE  */
    TOKEN_KEYWORD_FUNCTION = 284,  /* TOKEN_KEYWORD_FUNCTION  */
    TOKEN_IDENT = 285,             /* TOKEN_IDENT  */
    TOKEN_STRING = 286,            /* TOKEN_STRING  */
    TOKEN_INTEGER = 287,           /* TOKEN_INTEGER  */
    TOKEN_PICTURE = 288,           /* TOKEN_PICTURE  */
    TOKEN_ALPHANUMERIC = 289,      /* TOKEN_ALPHANUMERIC  */
    TOKEN_NUMERIC = 290,           /* TOKEN_NUMERIC  */
    TOKEN_SIGNED_NUMERIC = 291,    /* TOKEN_SIGNED_NUMERIC  */
    TOKEN_IMPLIED_DECIMAL = 292,   /* TOKEN_IMPLIED_DECIMAL  */
    TOKEN_COMPUTATION_LEVEL_0 = 293, /* TOKEN_COMPUTATION_LEVEL_0  */
    TOKEN_COMPUTATION_LEVEL_1 = 294, /* TOKEN_COMPUTATION_LEVEL_1  */
    TOKEN_COMPUTATION_LEVEL_2 = 295, /* TOKEN_COMPUTATION_LEVEL_2  */
    TOKEN_COMPUTATION_LEVEL_3 = 296, /* TOKEN_COMPUTATION_LEVEL_3  */
    TOKEN_LEFT_PARENTHESIS = 297,  /* TOKEN_LEFT_PARENTHESIS  */
    TOKEN_RIGHT_PARENTHESIS = 298, /* TOKEN_RIGHT_PARENTHESIS  */
    TOKEN_DOT = 299,               /* TOKEN_DOT  */
    TOKEN_COMMENT = 300,           /* TOKEN_COMMENT  */
    TOKEN_ADD = 301,               /* TOKEN_ADD  */
    TOKEN_SUB = 302,               /* TOKEN_SUB  */
    TOKEN_MULTIPLY = 303,          /* TOKEN_MULTIPLY  */
    TOKEN_DIVIDE = 304,            /* TOKEN_DIVIDE  */
    TOKEN_EQUAL = 305,             /* TOKEN_EQUAL  */
    TOKEN_GREATER_THAN = 306,      /* TOKEN_GREATER_THAN  */
    TOKEN_LESS_THAN = 307,         /* TOKEN_LESS_THAN  */
    TOKEN_EXPONENTIAL = 308,       /* TOKEN_EXPONENTIAL  */
    TOKEN_DISPLAY = 309            /* TOKEN_DISPLAY  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_TOKEN_H_INCLUDED  */
