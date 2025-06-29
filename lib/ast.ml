(*
 * The idea is that the AST should contain all the structural info,
 * but while building it from the cst, we do arity and type checking,
 * and associate variable references to slots in binding scopes.
 * *)

type name = Cst.name

type type_t = Cst.type_t

type binding = {
  name : name
  tp : type_t
  slot : int
}

type v_ref = {
  name : name
  tp : type_t
  scope_depth : int             (* 0 for closest scope *)
  slot : int
}

type func = {
  n_slots : int
  arity : int
  args : binding list
  captures : binding list
  body : expr
}

type let_block = {
  n_slots : int
  binds : binding list
  body : expr
}

type application = {
  func : expr
  args : expr list
}

type literal = {
  tp : type_t
  value : int
}

type expr =
  | Fun of func
  | Let of let_block
  | Var of v_ref
  | App of application
  | Lit of literal
