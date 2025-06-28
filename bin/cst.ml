(* Make AST handle type checking, closures, and var linking *)
type type_t =
  | Int_t
  | Fun_t of type_t * type_t

type name = Name of string

type binding = name * type_t

type expr =
  | Fun of binding list * expr
  | Let of binding list * expr
  | Var of name
  | App of expr * expr list
  | Lit of int
