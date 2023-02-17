(* File syntax.ml *)

type loc = string
let string_of_loc x = x

let group x1 y x2 = "(" ^ x1 ^ " " ^ y ^ " " ^ x2 ^ ")"

type aval = int
type aop = Sum | Sub | Mul | Mod | Div
type aexp =
| Int of aval
| Var of loc
| Aop of aop * aexp * aexp

let string_of_aop = function
| Sum -> "+"
| Sub -> "-"
| Mul -> "*"
| Mod -> "%"
| Div -> "/"
let rec string_of_aexp = function
| Int n -> string_of_int n
| Var x -> string_of_loc x
| Aop (op, a1, a2) -> group (string_of_aexp a1) (string_of_aop op) (string_of_aexp a2)

type env = loc -> aval
let aop_eval = function
  | Sum -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Mod -> (mod)
  | Div -> (/)
let rec aeval (a : aexp) (s : env) =
  match a with
  | Int n -> n
  | Var x -> s x
  | Aop (op, a1, a2) -> aop_eval op (aeval a1 s) (aeval a2 s)

(* Test de string_of_aexp *)
(* let () = print_endline (string_of_aexp (Sum (Var 0, Mul (Int 3, Var 2)))) *)

type bval = bool
type arel = Eq | Le
type brel = And | Or
type bexp =
| Bool of bval
| Arel of arel * aexp * aexp
| Brel of brel * bexp * bexp
| Neg of bexp

let string_of_arel = function
  | Eq -> "="
  | Le -> "<="
let string_of_brel = function
  | And -> "&&"
  | Or -> "||"
let rec string_of_bexp = function
  | Bool t -> if t then "true" else "false"
  | Arel (rel, a1, a2) -> group (string_of_aexp a1) (string_of_arel rel) (string_of_aexp a2)
  | Brel (rel, b1, b2) -> group (string_of_bexp b1) (string_of_brel rel) (string_of_bexp b2)
  | Neg b -> "~" ^ (string_of_bexp b)

let arel_eval = function
  | Eq -> (fun x y -> x = y)
  | Le -> (fun x y -> x <= y)
let brel_eval = function
  | And -> (fun x y -> x && y)
  | Or -> (fun x y -> x || y)
let rec beval (b : bexp) (s : env) =
  match b with
  | Bool b -> b
  | Arel (rel, a1, a2) -> arel_eval rel (aeval a1 s) (aeval a2 s)
  | Brel (rel, b1, b2) -> brel_eval rel (beval b1 s) (beval b2 s)
  | Neg b -> not (beval b s)

(* Test de string_of_bexp *)
(* let () = print_endline (string_of_bexp (Neg (Le (Sum (Var 0, Int 3), Var 2)))) *)

type com =
| Skip
| Comp of com * com
| Assn of loc * aexp
| Cond of bexp * com * com
| Loop of bexp * com

let rec string_of_com = function
  | Skip -> "skip"
  | Comp (c1, c2) -> (string_of_com c1) ^ ";\n" ^ (string_of_com c2)
  | Assn (x, a) -> (string_of_loc x) ^ " := " ^ (string_of_aexp a)
  | Cond (b, c1, c2) -> "if " ^ (string_of_bexp b) ^ "then {\n" ^ (string_of_com c1) ^ "\nelse {" ^ (string_of_com c2) ^ "\n}"
  | Loop (b, c) -> "while " ^ (string_of_bexp b) ^ " do {\n" ^ (string_of_com c) ^ "\n}"

(* Test de string_of_com *)
(* let () = print_endline (string_of_com (Loop (Bool true, Skip)))
let () = print_endline (string_of_com (Comp (Loop (Bool true, Skip), Cond (Le (Sum (Var 0, Int 3), Var 2), Skip, Skip)))) *)

let sub (s : env) (m : aval) (x : loc) =
  fun y -> if x = y then m else s y

let rec ceval (c : com) (s : env) =
  match c with
  | Skip -> s
  | Comp (c1, c2) -> ceval c2 (ceval c1 s)
  | Assn (x, a) -> sub s (aeval a s) x
  | Cond (b, c1, c2) -> if beval b s then ceval c1 s else ceval c2 s
  | Loop (b, c1) -> if beval b s then ceval c (ceval c1 s) else s

let compute c input = ceval c (fun x -> if x = "in" then input else 0) "out"

let affint n = print_int n; print_newline ()

let test _ =
  let sumUntil = Comp (Assn ("y", Int 0), Loop (Arel (Le, Int 0, Var "x"), Comp (Assn ("y", Aop (Sum, Var "x", Var "y")), Assn ("x", Aop (Sub, Var "x", Int 1))))) in
  for i = 1 to 5 do affint (ceval sumUntil (fun _ -> i) "y") done

(* let () =
  let w = Loop (Bool true, Skip) in
  affint (ceval w (fun x -> 0) 0) *)
