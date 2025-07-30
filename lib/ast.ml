(*
   Define basic structure as a recursive tree type
*)

open Name

type type_t =
  | Int_t
  | Fun_t of type_t array * type_t
  | Alias_t of name

type binding = name * type_t

type assignment =
  { lhs : binding
  ; rhs : expr
  }

and type_def =
  { lhs_t : name
  ; rhs_t : type_t
  }

and expr =
  | Fun of binding list * expr
  | Let of assignment list * expr
  | Var of name
  | App of expr * expr list
  | Lit of int

(* TODO module signatures.
 *
 * Think C header files, gives a set of type aliases and a set of
 * bindings such that any module with that signiture must contain a
 * set of type defs so that the names are a superset of those in the
 * sig, and the same with assignments and bindings.
 *
 * Basically if it in the signature then you know at least that much
 * is in the module.
 *
 * Also at least to start have modules w/ signitures not hide internal
 * details, ie you can reference things that aren't in the signiture
 * and type internals and stuff, again C style.
*)

type mod_t =
  { (* TODO mod level deps *)
    types : type_def list
  ; assigns : assignment list
  }

let fetch_alias s m = List.find_opt (fun td -> td.lhs_t = s) m.types

let rec fetch_nearest_alias s ms =
  match ms with
  | [] -> None
  | m :: rest ->
    (match fetch_alias s m with
     | Some v -> Some v
     | None -> fetch_nearest_alias s rest)
;;

let rec pp_type ?(mods = []) t =
  match t with
  | Int_t -> "Int"
  | Fun_t (args, result) ->
    Array.fold_left (fun acc arg -> acc ^ pp_type arg ^ " ") "(" args
    ^ pp_type result
    ^ ")"
  | Alias_t s ->
    let b = fetch_nearest_alias s mods in
    let prefix = "{ Alias : " ^ pp_name s ^ " bound to " in
    (match b with
     | None -> prefix ^ "nothing }"
     | Some td -> prefix ^ pp_type ~mods td.rhs_t ^ "}")
;;

let pp_type_def td = pp_name td.lhs_t ^ " := " ^ pp_type td.rhs_t
let pp_binding (n, t) = pp_name n ^ " : " ^ pp_type t

let rec pp_assignment a = pp_binding a.lhs ^ " = " ^ pp_expr a.rhs

and pp_expr e =
  match e with
  | Lit i -> string_of_int i
  | Var n -> pp_name n
  | App (f, aps) ->
    "( "
    ^ List.fold_left (fun acc arg -> acc ^ pp_expr arg ^ " ") (pp_expr f ^ " ") aps
    ^ ")"
  | Fun (binds, body) ->
    List.fold_left (fun acc b -> acc ^ pp_binding b ^ " ") "λ " binds
    ^ ". "
    ^ pp_expr body
  | Let (assigns, body) ->
    List.fold_left (fun acc a -> acc ^ pp_assignment a ^ "\n") "let\n" assigns
    ^ ". "
    ^ pp_expr body
;;

let pp_mod m =
  let prefix = "{Module with types:\n" in
  let types = List.fold_left (fun acc t -> acc ^ pp_type_def t ^ "\n") "" m.types in
  let assigns = List.fold_left (fun acc a -> acc ^ pp_assignment a ^ "\n") "" m.assigns in
  prefix ^ types ^ "And assignments:\n" ^ assigns ^ "}"
;;
