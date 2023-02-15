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
%type <aexp> aexpOperand

/* bexp */
%type <bexp> bexp
%token <bool> BOOL
%token AND OR
%token NEG
%token EQ LE
%type <bexp> bexpOperand

/* com */
%type <com> com
%token SKIP
%token COMP
%token ASSN
%token IF THEN ELSE
%token WHILE DO

%type<com> comList
%type<com> comListParen

%type <aop> aop
%type <arel> arel
%type <brel> brel

%token EOF
%start main
%type <Syntax.com> main

%%
main:
    comList EOF                     { $1 }

com:
    LPAREN com RPAREN                               { $2 }
  | SKIP                                            { Skip }
  | STRING ASSN aexp                                { Assn ($1, $3) }
  | IF bexp THEN comListParen ELSE comListParen     { Cond ($2, $4, $6) }
  | IF bexp THEN comListParen                       { Cond ($2, $4, Skip) }
  | WHILE bexp DO comListParen                      { Loop ($2, $4) }

comListParen:
    LPAREN comList RPAREN           { $2 }
  
comList:
    com                             { $1 }
  | com COMP comList                { Comp ($1, $3) }

aexp:
    aexpOperand                     { $1 }
  | aexpOperand aop aexpOperand     { Aop ($2, $1, $3) }

aexpOperand:
    LPAREN aexp RPAREN              { $2 }
  | INT                             { Int $1 }
  | STRING                          { Var $1 }

bexp:
    bexpOperand                     { $1 }
  | aexp arel aexp                  { Arel ($2, $1, $3) }
  | bexpOperand brel bexpOperand    { Brel ($2, $1, $3) }
  | NEG bexp                        { Neg $2 }

bexpOperand:
    LPAREN bexp RPAREN              { $2 }
  | BOOL                            { Bool $1 }

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
