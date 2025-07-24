(*
   Define basic structure as a recursive tree type
*)

(* Make AST handle type checking, closures, and var linking *)
type type_t =
  | Int_t
  | Fun_t of type_t array * type_t

type name = string
type binding = name * type_t

type expr =
  | Fun of binding list * expr
  | Let of (binding * expr) list * expr
  | Var of name
  | App of expr * expr list
  | Lit of int

let rec pp_type t =
  match t with
  | Int_t -> "Int"
  | Fun_t (args, result) ->
    Array.fold_left (fun acc arg -> acc ^ pp_type arg ^ " ") "(" args
    ^ ") "
    ^ pp_type result
;;

let pp_name s = s
let pp_binding (n, t) = pp_name n ^ ":" ^ pp_type t

let rec pp_expr e =
  match e with
  | Lit i -> string_of_int i
  | Var n -> pp_name n
  | App (f, aps) ->
    "(" ^ List.fold_left (fun acc arg -> acc ^ pp_expr arg ^ " ") (pp_expr f) aps ^ ")"
  | Fun (binds, body) ->
    "λ"
    ^ List.fold_left (fun acc b -> acc ^ pp_binding b ^ " ") "" binds
    ^ ". "
    ^ pp_expr body
  | Let (binds, body) ->
    "let"
    ^ List.fold_left
        (fun acc (b, v) -> acc ^ pp_binding b ^ " = " ^ pp_expr v ^ "\n ")
        ""
        binds
    ^ ". "
    ^ pp_expr body
;;
