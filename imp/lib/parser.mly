/* File parser.mly */

%{
open Syntax
%}

%token LPAREN RPAREN

/* aexp */
%type <aexp> aexp
%token <int> INT
%token <string> STRING
%token SUM
%token SUB
%token MUL

%left SUM SUB
%left MUL

/* bexp */
%type <bexp> bexp
%token <bool> BOOL
%token AND OR
%token NEG
%token EQ LE

%left AND OR
%left NEG

/* com */
%type <com> com
%token SKIP
%token COMP
%token ASSN
%token IF THEN ELSE
%token WHILE DO

%left COMP
%left ELSE DO

%type <aop> aop
%type <arel> arel
%type <brel> brel

%token EOF
%start main
%type <Syntax.com> main

%%
main:
    com EOF                     { $1 }

com:
    LPAREN com RPAREN           { $2 }
  | SKIP                        { Skip }
  | STRING ASSN aexp            { Assn ($1, $3) }
  | IF bexp THEN com ELSE com   { Cond ($2, $4, $6) }
  | WHILE bexp DO com           { Loop ($2, $4) }
  | com COMP com                { Comp ($1, $3) }

aexp:
    LPAREN aexp RPAREN          { $2 }
  | INT                         { Int $1 }
  | STRING                      { Var $1 }
  | aexp aop aexp               { Aop ($2, $1, $3) }

bexp:
    LPAREN bexp RPAREN          { $2 }
  | BOOL                        { Bool $1 }
  | aexp arel aexp              { Arel ($2, $1, $3) }
  | bexp brel bexp              { Brel ($2, $1, $3) }
  | NEG bexp                    { Neg $2 }

%inline aop:
| SUM { Sum }
| SUB { Sub }
| MUL { Mul }

%inline arel:
| EQ { Eq }
| LE { Le }

%inline brel:
| AND { And }
| OR { Or }
