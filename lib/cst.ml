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
