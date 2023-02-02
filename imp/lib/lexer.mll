(* File lexer.mll *)

{
open Parser        (* The type token is defined in parser.mli *)
}

rule token = parse
    [' ' '\t' '\n']                     { token lexbuf }     (* skip blanks *)
  | ['(' '{']                           { LPAREN }
  | [')' '}']                           { RPAREN }
  | eof                                 { EOF }
  (* aexp *)
  | ['0'-'9']+ as lxm                   { INT(int_of_string lxm) }
  | '+'                                 { SUM }
  | '-'                                 { SUB }
  | '*'                                 { MUL }
  (* bexp *)
  | "true"                              { BOOL(true) }
  | "false"                             { BOOL(false) }
  | '='                                 { EQ }
  | "<="                                { LE }
  | "&&"                                { AND }
  | "||"                                { OR }
  | '~'                                 { NEG }
  (* com *)
  | "skip"                              { SKIP }
  | ":="                                { ASSN }
  | ';'                                 { COMP }
  | "if"                                { IF }
  | "then"                              { THEN }
  | "else"                              { ELSE }
  | "while"                             { WHILE }
  | "do"                                { DO }
  | ['a'-'z' 'A'-'Z' '_']+ as lxm       { STRING(lxm) }     (* À la fin ! *)