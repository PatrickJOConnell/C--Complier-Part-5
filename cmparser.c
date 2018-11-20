/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_ELSE = 258,
     TOK_IF = 259,
     TOK_RETURN = 260,
     TOK_VOID = 261,
     TOK_INT = 262,
     TOK_WHILE = 263,
     TOK_PLUS = 264,
     TOK_MINUS = 265,
     TOK_MULT = 266,
     TOK_DIV = 267,
     TOK_LT = 268,
     TOK_LE = 269,
     TOK_GT = 270,
     TOK_GE = 271,
     TOK_EQ = 272,
     TOK_NE = 273,
     TOK_ASSIGN = 274,
     TOK_SEMI = 275,
     TOK_COMMA = 276,
     TOK_LPAREN = 277,
     TOK_RPAREN = 278,
     TOK_LSQ = 279,
     TOK_RSQ = 280,
     TOK_LBRACE = 281,
     TOK_RBRACE = 282,
     TOK_ERROR = 283,
     TOK_ID = 284,
     TOK_NUM = 285,
     TOK_SUB = 286
   };
#endif
/* Tokens.  */
#define TOK_ELSE 258
#define TOK_IF 259
#define TOK_RETURN 260
#define TOK_VOID 261
#define TOK_INT 262
#define TOK_WHILE 263
#define TOK_PLUS 264
#define TOK_MINUS 265
#define TOK_MULT 266
#define TOK_DIV 267
#define TOK_LT 268
#define TOK_LE 269
#define TOK_GT 270
#define TOK_GE 271
#define TOK_EQ 272
#define TOK_NE 273
#define TOK_ASSIGN 274
#define TOK_SEMI 275
#define TOK_COMMA 276
#define TOK_LPAREN 277
#define TOK_RPAREN 278
#define TOK_LSQ 279
#define TOK_RSQ 280
#define TOK_LBRACE 281
#define TOK_RBRACE 282
#define TOK_ERROR 283
#define TOK_ID 284
#define TOK_NUM 285
#define TOK_SUB 286




/* Copy the first part of user declarations.  */
#line 1 "cmparser.y"


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ast.h"
#include "symbolTable.h"
#include "util.h"


	/* other external function prototypes */
	extern int yylex();
	extern int initLex(int ,  char **);
	extern AstNodePtr new_Node(NodeKind kind); 

	/* external global variables */

	extern int		yydebug;
	extern int		yylineno;
	extern SymbolTableStackEntryPtr symbolStackTop;
	extern int scopeDepth;
extern int typecheck();
	/* function prototypes */ 
	void	yyerror(const char *);

	/* global variables */

	AstNodePtr  program;
	FILE *outfile; 

	

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 35 "cmparser.y"
{
	AstNodePtr nodePtr;
	int        iVal;
	char      *cVal;
	Type      *type;
}
/* Line 193 of yacc.c.  */
#line 197 "cmparser.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 210 "cmparser.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   117

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  32
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  47
/* YYNRULES -- Number of rules.  */
#define YYNRULES  81
/* YYNRULES -- Number of states.  */
#define YYNSTATES  133

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   286

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    12,    15,    19,    26,
      27,    28,    37,    39,    41,    45,    47,    50,    55,    57,
      59,    60,    61,    67,    68,    69,    76,    79,    81,    84,
      85,    87,    89,    91,    93,    95,    98,   100,   102,   106,
     107,   114,   115,   122,   126,   129,   130,   135,   137,   139,
     144,   145,   150,   151,   156,   157,   162,   163,   168,   169,
     174,   175,   180,   182,   183,   188,   189,   194,   196,   197,
     202,   203,   208,   210,   214,   216,   218,   220,   225,   227,
     228,   232
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      33,     0,    -1,    34,    -1,    35,    -1,    36,    34,    -1,
      37,    -1,    37,    35,    -1,    43,    29,    20,    -1,    43,
      29,    24,    30,    25,    20,    -1,    -1,    -1,    43,    29,
      22,    38,    40,    23,    39,    44,    -1,    41,    -1,     6,
      -1,    41,    21,    42,    -1,    42,    -1,    43,    29,    -1,
      43,    29,    24,    25,    -1,     7,    -1,     6,    -1,    -1,
      -1,    26,    45,    50,    46,    27,    -1,    -1,    -1,    26,
      47,    49,    50,    48,    27,    -1,    36,    49,    -1,    36,
      -1,    51,    50,    -1,    -1,    52,    -1,    44,    -1,    53,
      -1,    56,    -1,    58,    -1,    59,    20,    -1,    20,    -1,
      54,    -1,    54,     3,    51,    -1,    -1,     4,    22,    59,
      55,    23,    51,    -1,    -1,     8,    22,    59,    57,    23,
      51,    -1,     5,    59,    20,    -1,     5,    20,    -1,    -1,
      61,    19,    60,    59,    -1,    62,    -1,    29,    -1,    29,
      24,    59,    25,    -1,    -1,    69,    15,    63,    69,    -1,
      -1,    69,    13,    64,    69,    -1,    -1,    69,    16,    65,
      69,    -1,    -1,    69,    14,    66,    69,    -1,    -1,    69,
      17,    67,    69,    -1,    -1,    69,    18,    68,    69,    -1,
      69,    -1,    -1,    69,     9,    70,    72,    -1,    -1,    69,
      10,    71,    72,    -1,    72,    -1,    -1,    72,    11,    73,
      75,    -1,    -1,    72,    12,    74,    75,    -1,    75,    -1,
      22,    59,    23,    -1,    61,    -1,    76,    -1,    30,    -1,
      29,    22,    77,    23,    -1,    78,    -1,    -1,    78,    21,
      59,    -1,    59,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    74,    74,    77,    78,    81,    82,    88,   103,   123,
     143,   123,   155,   156,   159,   167,   173,   194,   218,   221,
     226,   232,   226,   238,   244,   238,   274,   275,   278,   282,
     285,   288,   291,   292,   295,   298,   303,   310,   313,   319,
     319,   330,   330,   342,   347,   354,   354,   365,   370,   381,
     396,   396,   405,   405,   414,   414,   424,   424,   434,   434,
     444,   444,   453,   456,   456,   466,   466,   476,   481,   481,
     491,   491,   500,   505,   506,   507,   508,   515,   530,   531,
     534,   543
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ELSE", "TOK_IF", "TOK_RETURN",
  "TOK_VOID", "TOK_INT", "TOK_WHILE", "TOK_PLUS", "TOK_MINUS", "TOK_MULT",
  "TOK_DIV", "TOK_LT", "TOK_LE", "TOK_GT", "TOK_GE", "TOK_EQ", "TOK_NE",
  "TOK_ASSIGN", "TOK_SEMI", "TOK_COMMA", "TOK_LPAREN", "TOK_RPAREN",
  "TOK_LSQ", "TOK_RSQ", "TOK_LBRACE", "TOK_RBRACE", "TOK_ERROR", "TOK_ID",
  "TOK_NUM", "TOK_SUB", "$accept", "Start", "Declarations", "Functions",
  "Var_Declaration", "Fun_Declaration", "@1", "@2", "Params", "Param_List",
  "Param", "Type_Specifier", "Compound_Stmt", "@3", "@4", "@5", "@6",
  "Local_Declarations", "Statements", "Statement", "Expr_Statement",
  "Selection_Stmt", "If_Else_Statement", "@11", "Iteration_Stmt", "@12",
  "Return_Stmt", "Expression", "@13", "Var", "Simple_Expression", "@14",
  "@15", "@16", "@17", "@18", "@19", "Additive_Expression", "@20", "@21",
  "Term", "@22", "@23", "Factor", "Call", "Args", "Args_List", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    32,    33,    34,    34,    35,    35,    36,    36,    38,
      39,    37,    40,    40,    41,    41,    42,    42,    43,    43,
      45,    46,    44,    47,    48,    44,    49,    49,    50,    50,
      51,    51,    51,    51,    51,    52,    52,    53,    53,    55,
      54,    57,    56,    58,    58,    60,    59,    59,    61,    61,
      63,    62,    64,    62,    65,    62,    66,    62,    67,    62,
      68,    62,    62,    70,    69,    71,    69,    69,    73,    72,
      74,    72,    72,    75,    75,    75,    75,    76,    77,    77,
      78,    78
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     2,     3,     6,     0,
       0,     8,     1,     1,     3,     1,     2,     4,     1,     1,
       0,     0,     5,     0,     0,     6,     2,     1,     2,     0,
       1,     1,     1,     1,     1,     2,     1,     1,     3,     0,
       6,     0,     6,     3,     2,     0,     4,     1,     1,     4,
       0,     4,     0,     4,     0,     4,     0,     4,     0,     4,
       0,     4,     1,     0,     4,     0,     4,     1,     0,     4,
       0,     4,     1,     3,     1,     1,     1,     4,     1,     0,
       3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    19,    18,     0,     2,     3,     0,     5,     0,     1,
       4,     6,     0,     0,     0,     7,     9,     0,     0,     0,
      13,     0,    12,    15,     0,     0,    10,     0,    16,     8,
       0,    14,     0,    20,    11,    17,    29,     0,     0,     0,
       0,    36,     0,    48,    76,    31,    21,    29,    30,    32,
      37,    33,    34,     0,    74,    47,    62,    67,    72,    75,
      27,     0,    29,     0,    44,     0,     0,     0,    79,     0,
       0,    28,     0,    35,    45,    63,    65,    52,    56,    50,
      54,    58,    60,    68,    70,    26,     0,    24,    39,    43,
      41,    73,    81,     0,    78,     0,    22,    38,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,     0,    49,    46,    74,    64,    66,    53,
      57,    51,    55,    59,    61,    69,    71,    25,     0,     0,
      80,    40,    42
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     3,     4,     5,     6,     7,    18,    30,    21,    22,
      23,     8,    45,    36,    70,    37,   109,    62,    46,    47,
      48,    49,    50,   110,    51,   111,    52,    53,    98,    54,
      55,   103,   101,   104,   102,   105,   106,    56,    99,   100,
      57,   107,   108,    58,    59,    93,    94
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -72
static const yytype_int8 yypact[] =
{
      19,   -72,   -72,    20,   -72,   -72,    19,    19,    -6,   -72,
     -72,   -72,    25,    14,    -8,   -72,   -72,    26,    46,    61,
      52,    64,    67,   -72,    60,    70,   -72,    19,    68,   -72,
      65,   -72,    69,    56,   -72,   -72,    11,    19,    71,   -12,
      73,   -72,   -18,   -15,   -72,   -72,   -72,    11,   -72,   -72,
      93,   -72,   -72,    77,    79,   -72,    51,    59,   -72,   -72,
      19,    72,    11,   -18,   -72,    80,   -18,    76,   -18,   -18,
      75,   -72,    11,   -72,   -72,   -72,   -72,   -72,   -72,   -72,
     -72,   -72,   -72,   -72,   -72,   -72,    15,   -72,   -72,   -72,
     -72,   -72,   -72,    81,    82,    83,   -72,   -72,   -18,   -18,
     -18,   -18,   -18,   -18,   -18,   -18,   -18,   -18,   -18,    78,
      84,    86,   -72,   -18,   -72,   -72,   -72,    59,    59,    63,
      63,    63,    63,    63,    63,   -72,   -72,   -72,    11,    11,
     -72,   -72,   -72
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -72,   -72,   100,   103,   -32,   -72,   -72,   -72,   -72,   -72,
      85,    -5,    87,   -72,   -72,   -72,   -72,    53,   -41,   -71,
     -72,   -72,   -72,   -72,   -72,   -72,   -72,   -39,   -72,   -57,
     -72,   -72,   -72,   -72,   -72,   -72,   -72,   -26,   -72,   -72,
     -17,   -72,   -72,   -23,   -72,   -72,   -72
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -24
static const yytype_int16 yytable[] =
{
      65,    97,    12,    67,    42,    60,    71,    68,    64,    69,
      42,    43,    44,    24,    16,    38,    39,    43,    44,    40,
       9,    87,    24,    13,    88,     1,     2,    90,    60,    92,
      95,    41,    61,    42,    15,    15,    16,    33,    17,    17,
      43,    44,   116,   116,   116,   116,   116,   116,   116,   116,
     116,   116,    20,     2,    14,    61,    19,   131,   132,   115,
      75,    76,   -23,   -23,    77,    78,    79,    80,    81,    82,
      83,    84,    75,    76,   130,   119,   120,   121,   122,   123,
     124,   -19,   117,   118,   125,   126,    25,    26,    27,    28,
      29,    33,    32,    63,    35,    66,    72,    73,    74,    91,
      89,    86,    96,   113,   112,   127,    10,   128,   114,   129,
      11,     0,    31,    85,     0,     0,     0,    34
};

static const yytype_int16 yycheck[] =
{
      39,    72,     7,    42,    22,    37,    47,    22,    20,    24,
      22,    29,    30,    18,    22,     4,     5,    29,    30,     8,
       0,    62,    27,    29,    63,     6,     7,    66,    60,    68,
      69,    20,    37,    22,    20,    20,    22,    26,    24,    24,
      29,    30,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,     6,     7,    29,    60,    30,   128,   129,    98,
       9,    10,     6,     7,    13,    14,    15,    16,    17,    18,
      11,    12,     9,    10,   113,   101,   102,   103,   104,   105,
     106,    29,    99,   100,   107,   108,    25,    23,    21,    29,
      20,    26,    24,    22,    25,    22,     3,    20,    19,    23,
      20,    29,    27,    21,    23,    27,     6,    23,    25,    23,
       7,    -1,    27,    60,    -1,    -1,    -1,    30
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,     7,    33,    34,    35,    36,    37,    43,     0,
      34,    35,    43,    29,    29,    20,    22,    24,    38,    30,
       6,    40,    41,    42,    43,    25,    23,    21,    29,    20,
      39,    42,    24,    26,    44,    25,    45,    47,     4,     5,
       8,    20,    22,    29,    30,    44,    50,    51,    52,    53,
      54,    56,    58,    59,    61,    62,    69,    72,    75,    76,
      36,    43,    49,    22,    20,    59,    22,    59,    22,    24,
      46,    50,     3,    20,    19,     9,    10,    13,    14,    15,
      16,    17,    18,    11,    12,    49,    29,    50,    59,    20,
      59,    23,    59,    77,    78,    59,    27,    51,    60,    70,
      71,    64,    66,    63,    65,    67,    68,    73,    74,    48,
      55,    57,    23,    21,    25,    59,    61,    72,    72,    69,
      69,    69,    69,    69,    69,    75,    75,    27,    23,    23,
      59,    51,    51
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 74 "cmparser.y"
    {;}
    break;

  case 3:
#line 77 "cmparser.y"
    { program = (yyvsp[(1) - (1)].nodePtr);;}
    break;

  case 4:
#line 78 "cmparser.y"
    { ;}
    break;

  case 5:
#line 81 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr) ;}
    break;

  case 6:
#line 82 "cmparser.y"
    { 
	(yyvsp[(1) - (2)].nodePtr)->sibling = (yyvsp[(2) - (2)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(1) - (2)].nodePtr) 	
;}
    break;

  case 7:
#line 88 "cmparser.y"
    {	
			  if ((yyvsp[(1) - (3)].type)->kind != INT){	//C-- deals only with integers
				  yyerror("non-int variable declared\n");
				  yyerror((yyvsp[(2) - (3)].cVal));	
			  }	
			  ElementPtr symElement = symLookup((yyvsp[(2) - (3)].cVal)); 
			  if(!symElement || scopeDepth != symElement->scope) 
				  symInsert((yyvsp[(2) - (3)].cVal),(yyvsp[(1) - (3)].type),yylineno);		
			  else 
			  {
				  yyerror("Redeclaration of variable"); 
				  yyerror((yyvsp[(2) - (3)].cVal));
				  free((yyvsp[(1) - (3)].type));	
			  }
		  ;}
    break;

  case 8:
#line 103 "cmparser.y"
    { 
	if ((yyvsp[(1) - (6)].type)->kind != INT){ //C-- should have only integer types
		yyerror("non-int variable declared\n");
		yyerror((yyvsp[(2) - (6)].cVal));
	}	
	ElementPtr symElement = symLookup((yyvsp[(2) - (6)].cVal)); //look up identifier in symbol table
	if(!symElement || scopeDepth != symElement->scope) {	/* reuse Type for storing dimension */
		(yyvsp[(1) - (6)].type)->kind = ARRAY; 
		(yyvsp[(1) - (6)].type)->dimension = (yyvsp[(4) - (6)].iVal); 
		(yyvsp[(1) - (6)].type)->function = NULL; 
		symInsert((yyvsp[(2) - (6)].cVal),(yyvsp[(1) - (6)].type),yylineno); //insert it in symbol table
	}
	else {
		yyerror("Redeclaration of variable");
		yyerror((yyvsp[(2) - (6)].cVal));
		free((yyvsp[(1) - (6)].type));
	} 	
;}
    break;

  case 9:
#line 123 "cmparser.y"
    {	
			  (yyval.nodePtr) =  new_Node(METHOD); 	 //Mid-rule action. It counts as a component of the rule. It has not
			  (yyval.nodePtr)->nLinenumber = yylineno; //been defined in the token and non-terminal declarations, so its type is set using 
			  //the $<...>$ syntax. $<nodePtr>$ means that the datatype of the value
			  (yyval.nodePtr)->nType = (yyvsp[(1) - (3)].type);	     	 //represented by this component is nodePtr 
			  ElementPtr symElement = symLookup((yyvsp[(2) - (3)].cVal)); //the identifier is looked up to see if it exists. If it does not exist,
			  if(!symElement || scopeDepth != symElement->scope) { 	 //or if it has been declared in a previous scope 
				  (yyval.nodePtr)->nSymbolPtr = symInsert((yyvsp[(2) - (3)].cVal),(yyvsp[(1) - (3)].type),yylineno); //insert it in the symbol table, set the nSymbolPtr
				  (yyval.nodePtr)->nSymbolPtr->stype->function = (Type*) malloc(sizeof(Type)); //allocate the type for the function
				  (yyval.nodePtr)->nSymbolPtr->stype->function->kind = (yyvsp[(1) - (3)].type)->kind; 	//set the kind of the function, i.e. its return type
				  (yyval.nodePtr)->nSymbolPtr->snode = (yyval.nodePtr); 			 //nSymbolPtr->snode must point to the AST node itself
				  (yyvsp[(1) - (3)].type)->kind = FUNCTION; 								//$1->kind is a function.
				  enterScope(); 										//create a new scope for the function 
			  }	
			  else {
				  yyerror("Redeclaration of function");
				  yyerror((yyvsp[(2) - (3)].cVal));
				  free((yyvsp[(1) - (3)].type));
			  }  
		  ;}
    break;

  case 10:
#line 143 "cmparser.y"
    {
	(yyvsp[(4) - (6)].nodePtr)->children[0] = (yyvsp[(5) - (6)].nodePtr); 	//refer to the previous semantic action node with $<nodePtr>4, 
	//since it is the fourth component of the rule
;}
    break;

  case 11:
#line 147 "cmparser.y"
    { /* changed from Compound_Stmt_Function to Compount_Stmt */
	(yyvsp[(4) - (8)].nodePtr)->children[1] = (yyvsp[(8) - (8)].nodePtr); 	//Compound Statement. According to the handout, the children[1] should be set to Compund_Stmt.
	(yyval.nodePtr) = (yyvsp[(4) - (8)].nodePtr); //this is the last action, $$, i.e. Fun_Declaration is set to the node created in the first semantic action, which represents
	leaveScope(); /* added to pop off the scope entered earlier in Fun_Declaration */
;}
    break;

  case 12:
#line 155 "cmparser.y"
    {(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);}
    break;

  case 13:
#line 156 "cmparser.y"
    {(yyval.nodePtr)=NULL;;}
    break;

  case 14:
#line 159 "cmparser.y"
    { 
		     (yyval.nodePtr) = (yyvsp[(1) - (3)].nodePtr); //this is a paramter list appearing in a function declaration. Mid-rule action. Set its value to $1. 
		     while((yyval.nodePtr)->sibling)	//go to the end of the list of siblings
			     (yyval.nodePtr) = (yyval.nodePtr)->sibling; 
		     (yyval.nodePtr)->sibling = (yyvsp[(3) - (3)].nodePtr); //set the sibling to $3, that is to the next Param in input. This whole list (current $3 included) will represent Param_List
		     (yyvsp[(3) - (3)].nodePtr)->sibling = NULL;	   //so far if there are even more params in input, ie. as long as this rule keeps matching.
		     (yyval.nodePtr) = (yyvsp[(1) - (3)].nodePtr);
	     ;}
    break;

  case 15:
#line 167 "cmparser.y"
    {
	(yyvsp[(1) - (1)].nodePtr)->sibling = NULL;	//single parameter. It has no siblings   		
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);	   		
;}
    break;

  case 16:
#line 173 "cmparser.y"
    { 	
		ElementPtr symElement = symLookup((yyvsp[(2) - (2)].cVal)); //lookup if it has been declared before
		if(!symElement || scopeDepth != symElement->scope)// if not declared before, or declared in a different scope, it must be inserted in the current (i.e. function) 												  //scope
		{
			(yyval.nodePtr) =  new_Node(FORMALVAR);	//this is a formal variable
			(yyval.nodePtr)->nLinenumber = yylineno;	//set the line number
			if ((yyvsp[(1) - (2)].type)->kind != INT){		//if it is not INT, C-- error
				yyerror("non-int variable declared\n");
			}
			else{
				(yyval.nodePtr)->nSymbolPtr = symInsert((yyvsp[(2) - (2)].cVal),(yyvsp[(1) - (2)].type),yylineno); //otherwise insert in symbol table, set nSymbolPtr to the entry in the symbol table
				(yyval.nodePtr)->nType = (yyvsp[(1) - (2)].type);	//set the type of the whole Param to $1 (i.e. Type_Specifier)
			}
		}	
		else 
		{
			yyerror("Redeclaration of variable");
			yyerror((yyvsp[(2) - (2)].cVal));
			free((yyvsp[(1) - (2)].type));
		}
	;}
    break;

  case 17:
#line 194 "cmparser.y"
    {
	ElementPtr symElement = symLookup((yyvsp[(2) - (4)].cVal));
	if(!symElement || scopeDepth != symElement->scope)
	{
		Type *typ;	
		if ((yyvsp[(1) - (4)].type)->kind != INT){
			yyerror("non-int array declared\n");
		}
		typ = new_type(ARRAY);
		(yyval.nodePtr) =  new_Node(FORMALVAR);
		(yyval.nodePtr)->nLinenumber = yylineno;
		(yyval.nodePtr)->nSymbolPtr = symInsert((yyvsp[(2) - (4)].cVal),typ,yylineno);
		(yyval.nodePtr)->nType = typ;
		free((yyvsp[(1) - (4)].type));
	}	
	else 
	{
		yyerror("Redeclaration of variable\n");
		yyerror((yyvsp[(2) - (4)].cVal));
		free((yyvsp[(1) - (4)].type));
	}
;}
    break;

  case 18:
#line 218 "cmparser.y"
    {
			 (yyval.type) = (Type*) new_type(INT); //creates a new Type INT struct, it is used elsewhere whenever Type_Specifier appears (?)
		 ;}
    break;

  case 19:
#line 221 "cmparser.y"
    { 
	(yyval.type) = (Type*) new_type(VOID); //creates a new Type VOID struct
;}
    break;

  case 20:
#line 226 "cmparser.y"
    {  
			enterScope(); //a compound statement has a different scope from that of the function declaration, as per handout instructions
			(yyval.nodePtr) =  new_StmtNode(COMPOUND_STMT);//mid-rule action. Create a new StmtNode of kind Compund_Stmt. 
			(yyval.nodePtr)->nSymbolTabPtr = symbolStackTop->symbolTablePtr; //the hashtable with the Symbol Entries
			(yyval.nodePtr)->nLinenumber = yylineno;			   	//set the line number
		;}
    break;

  case 21:
#line 232 "cmparser.y"
    { (yyvsp[(2) - (3)].nodePtr)->children[0] = (yyvsp[(3) - (3)].nodePtr); ;}
    break;

  case 22:
#line 233 "cmparser.y"
    { 
	leaveScope(); //Statements will be parsed and the actions executed, then the scope ends.
	(yyval.nodePtr) = (yyvsp[(2) - (5)].nodePtr); //set $$ to the second node.
;}
    break;

  case 23:
#line 238 "cmparser.y"
    {  
	enterScope(); //same as the previous, only we have Local_Declarations also this time. Local_Declarations will be parsed and its actions executed. 
	(yyval.nodePtr) =  new_StmtNode(COMPOUND_STMT); //This seems like a bottom-up parser (?)
	(yyval.nodePtr)->nSymbolTabPtr = symbolStackTop->symbolTablePtr;
	(yyval.nodePtr)->nLinenumber = yylineno;			   	
;}
    break;

  case 24:
#line 244 "cmparser.y"
    { (yyvsp[(2) - (4)].nodePtr)->children[0] = (yyvsp[(4) - (4)].nodePtr);;}
    break;

  case 25:
#line 245 "cmparser.y"
    { 
	leaveScope();
	(yyval.nodePtr) = (yyvsp[(2) - (6)].nodePtr);
;}
    break;

  case 26:
#line 274 "cmparser.y"
    {;}
    break;

  case 27:
#line 275 "cmparser.y"
    {;}
    break;

  case 28:
#line 278 "cmparser.y"
    {
		     (yyvsp[(1) - (2)].nodePtr)->sibling = (yyvsp[(2) - (2)].nodePtr); //for every statement, set its sibling to the rest of the statements
		     (yyval.nodePtr) = (yyvsp[(1) - (2)].nodePtr);				    
	     ;}
    break;

  case 29:
#line 282 "cmparser.y"
    {(yyval.nodePtr)=NULL;}
    break;

  case 30:
#line 285 "cmparser.y"
    { 
		    (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);
	    ;}
    break;

  case 31:
#line 288 "cmparser.y"
    {
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);
;}
    break;

  case 32:
#line 291 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr); ;}
    break;

  case 33:
#line 292 "cmparser.y"
    {
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);
;}
    break;

  case 34:
#line 295 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr); ;}
    break;

  case 35:
#line 298 "cmparser.y"
    {	
			 (yyval.nodePtr) =  new_StmtNode(EXPRESSION_STMT);
			 (yyval.nodePtr)->nLinenumber = yylineno;
			 (yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);  
		 ;}
    break;

  case 36:
#line 303 "cmparser.y"
    {
	(yyval.nodePtr) =  new_StmtNode(EXPRESSION_STMT) ;
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = NULL;
;}
    break;

  case 37:
#line 310 "cmparser.y"
    { 
			 (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr); 
		 ;}
    break;

  case 38:
#line 313 "cmparser.y"
    { 
	(yyval.nodePtr) = (yyvsp[(1) - (3)].nodePtr);
	(yyval.nodePtr)->children[2]= (yyvsp[(3) - (3)].nodePtr);
;}
    break;

  case 39:
#line 319 "cmparser.y"
    {
			    (yyval.nodePtr) =  new_StmtNode(IF_THEN_ELSE_STMT);
			    (yyval.nodePtr)->nLinenumber = yylineno;
			    (yyval.nodePtr)->children[0] = (yyvsp[(3) - (3)].nodePtr);
		    ;}
    break;

  case 40:
#line 324 "cmparser.y"
    { 
	(yyvsp[(4) - (6)].nodePtr)->children[1] = (yyvsp[(6) - (6)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(4) - (6)].nodePtr);
;}
    break;

  case 41:
#line 330 "cmparser.y"
    {
			 (yyval.nodePtr) =  new_StmtNode(WHILE_STMT);
			 (yyval.nodePtr)->nLinenumber = yylineno;
			 (yyval.nodePtr)->children[0] = (yyvsp[(3) - (3)].nodePtr); 							
		 ;}
    break;

  case 42:
#line 335 "cmparser.y"
    {
	(yyvsp[(4) - (6)].nodePtr)->children[1] = (yyvsp[(6) - (6)].nodePtr); 
	(yyval.nodePtr) = (yyvsp[(4) - (6)].nodePtr);			
	(yyval.nodePtr)->nLinenumber = yylineno;		
;}
    break;

  case 43:
#line 342 "cmparser.y"
    { 	
		      (yyval.nodePtr) =  new_StmtNode(RETURN_STMT);
		      (yyval.nodePtr)->nLinenumber = yylineno;
		      (yyval.nodePtr)->children[0] = (yyvsp[(2) - (3)].nodePtr); 
	      ;}
    break;

  case 44:
#line 347 "cmparser.y"
    {
	(yyval.nodePtr) =  new_StmtNode(RETURN_STMT);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = NULL; 
;}
    break;

  case 45:
#line 354 "cmparser.y"
    {
		     (yyval.nodePtr) =  new_ExprNode(ASSI_EXP);
		     (yyval.nodePtr)->nLinenumber = yylineno;
		     (yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	     			
	     ;}
    break;

  case 46:
#line 359 "cmparser.y"
    {

	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);						
;}
    break;

  case 47:
#line 365 "cmparser.y"
    {
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);	
;}
    break;

  case 48:
#line 370 "cmparser.y"
    { 
	      (yyval.nodePtr) =  new_ExprNode(VAR_EXP); //create a new ExprNode to represent the variable
	      (yyval.nodePtr)->nLinenumber = yylineno;  
	      (yyval.nodePtr)->nSymbolPtr =  symLookup((yyvsp[(1) - (1)].cVal)); 
	      if(!(yyval.nodePtr)->nSymbolPtr)		
	      {
		      yyerror("Variable must be declared before use");
		      yyerror((yyvsp[(1) - (1)].cVal));
		      free((yyval.nodePtr));
	      }	
      ;}
    break;

  case 49:
#line 381 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(ARRAY_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->nSymbolPtr = symLookup((yyvsp[(1) - (4)].cVal));
	if(!(yyval.nodePtr)->nSymbolPtr)
	{  	
		yyerror("Variable must be declared before use");
		yyerror((yyvsp[(1) - (4)].cVal));
		free((yyval.nodePtr));
	}
	else
		(yyval.nodePtr)->children[0] = (yyvsp[(3) - (4)].nodePtr);			  	
;}
    break;

  case 50:
#line 396 "cmparser.y"
    {
			    (yyval.nodePtr) =  new_ExprNode(GT_EXP);
			    (yyval.nodePtr)->nLinenumber = yylineno;
			    (yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
		    ;}
    break;

  case 51:
#line 401 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);		
;}
    break;

  case 52:
#line 405 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(LT_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 53:
#line 410 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);		
;}
    break;

  case 54:
#line 414 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(GE_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 55:
#line 419 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	
;}
    break;

  case 56:
#line 424 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(LE_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 57:
#line 429 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	
;}
    break;

  case 58:
#line 434 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(EQ_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 59:
#line 439 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	
;}
    break;

  case 60:
#line 444 "cmparser.y"
    {	
	(yyval.nodePtr) =  new_ExprNode(NE_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 61:
#line 449 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	
;}
    break;

  case 62:
#line 453 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr) ;}
    break;

  case 63:
#line 456 "cmparser.y"
    {
			      (yyval.nodePtr) =  new_ExprNode(ADD_EXP);
			      (yyval.nodePtr)->nLinenumber = yylineno;
			      (yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
		      ;}
    break;

  case 64:
#line 461 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);
;}
    break;

  case 65:
#line 466 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(SUB_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 		
;}
    break;

  case 66:
#line 471 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	            	        
;}
    break;

  case 67:
#line 476 "cmparser.y"
    {
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);	
;}
    break;

  case 68:
#line 481 "cmparser.y"
    {
	       (yyval.nodePtr) = new_ExprNode(MULT_EXP);
	       (yyval.nodePtr)->nLinenumber = yylineno;
	       (yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 		
       ;}
    break;

  case 69:
#line 486 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	      
;}
    break;

  case 70:
#line 491 "cmparser.y"
    {
	(yyval.nodePtr) =  new_ExprNode(DIV_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->children[0] = (yyvsp[(1) - (2)].nodePtr);	 			
;}
    break;

  case 71:
#line 496 "cmparser.y"
    {
	(yyvsp[(3) - (4)].nodePtr)->children[1] = (yyvsp[(4) - (4)].nodePtr);
	(yyval.nodePtr) = (yyvsp[(3) - (4)].nodePtr);	      
;}
    break;

  case 72:
#line 500 "cmparser.y"
    {
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);		 	
;}
    break;

  case 73:
#line 505 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(2) - (3)].nodePtr); ;}
    break;

  case 74:
#line 506 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr) ;}
    break;

  case 75:
#line 507 "cmparser.y"
    { (yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr) ;}
    break;

  case 76:
#line 508 "cmparser.y"
    { 
	(yyval.nodePtr) =  new_ExprNode(CONST_EXP);
	(yyval.nodePtr)->nLinenumber = yylineno;
	(yyval.nodePtr)->nValue = (yyvsp[(1) - (1)].iVal); 
;}
    break;

  case 77:
#line 515 "cmparser.y"
    {
	       (yyval.nodePtr)= new_ExprNode(CALL_EXP);
	       (yyval.nodePtr)->nLinenumber = yylineno;
	       (yyval.nodePtr)->nSymbolPtr = symLookup((yyvsp[(1) - (4)].cVal));

	       if(!(yyval.nodePtr)->nSymbolPtr)
	       {  	
		       printf("function %s not found, forward reference\n", (yyvsp[(1) - (4)].cVal));
		       (yyval.nodePtr)->fname = strdup((yyvsp[(1) - (4)].cVal));
	       }
	       (yyval.nodePtr)->fname = strdup((yyvsp[(1) - (4)].cVal));
	       (yyval.nodePtr)->children[0] = (yyvsp[(3) - (4)].nodePtr);	
       ;}
    break;

  case 78:
#line 530 "cmparser.y"
    {(yyval.nodePtr)=(yyvsp[(1) - (1)].nodePtr);;}
    break;

  case 79:
#line 531 "cmparser.y"
    { (yyval.nodePtr) = NULL; ;}
    break;

  case 80:
#line 535 "cmparser.y"
    { 
	(yyval.nodePtr) = (yyvsp[(1) - (3)].nodePtr);
	while((yyval.nodePtr)->sibling)
		(yyval.nodePtr) = (yyval.nodePtr)->sibling;
	(yyval.nodePtr)->sibling = (yyvsp[(3) - (3)].nodePtr);
	(yyvsp[(3) - (3)].nodePtr)->sibling = NULL;
	(yyval.nodePtr) = (yyvsp[(1) - (3)].nodePtr);
;}
    break;

  case 81:
#line 543 "cmparser.y"
    {
	(yyvsp[(1) - (1)].nodePtr)->sibling = NULL;	   		
	(yyval.nodePtr) = (yyvsp[(1) - (1)].nodePtr);				
;}
    break;


/* Line 1267 of yacc.c.  */
#line 2238 "cmparser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 549 "cmparser.y"

void yyerror (char const *s) {
	fprintf (stderr, "Error at line %d: %s\n", yylineno, s);
	exit(1);
}


int main(int argc, char **argv){

	initLex(argc,argv);

#ifdef YYLLEXER
	while (gettok() !=0) ; //gettok returns 0 on EOF
	return;
#else
	initSymbolTable(); 	
	yyparse();
	typecheck();
	print_Ast();
#endif

} 


