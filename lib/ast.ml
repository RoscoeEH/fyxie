(* Define basic structure as a recursive tree type *)
open Option
    
open Name

open Types

type type_t = Types.ty    (* kind star *)

type binding = name * type_t

type pattern =
  { dtype : name
  ; matched : constructor
  ; p_binds : name option list
  }

type assignment =
  { lhs : binding
  ; rhs : expr
  }

and expr =
  | Fun of binding list * expr
  | Let of assignment list * expr
  | Var of name
  | App of expr * expr list
  | Lit of int
  | Dec of expr * (pattern * expr) list * expr option

type top_level =
  | TL_an of assignment
  | TL_ex of expr
  | TL_m  of mod_t
  | TL_dt of ty
      
and mod_t = (* TODO mod level deps *)
  { mod_name : name_atom option
  ; top : top_level list
  }

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
 * details, ie you can reference things that aren't in the signature
 * and type internals and stuff, again C style. 
*)

let mod_assigns m = 
  List.filter_map (fun tl ->
      match tl with
      | TL_an at -> some at
      | _ -> none) m.top
;;

let mod_exprs m = 
  List.filter_map (fun tl ->
      match tl with
      | TL_ex at -> some at
      | _ -> none) m.top
;;

let mod_submods m = 
  List.filter_map (fun tl ->
      match tl with
      | TL_m m -> some m
      | _ -> none) m.top
;;

module PrettyPrint = struct
  open Util.Pretty
  let pp_type = Types.pp_type

  let pp_binding (n, t) = pp_name n ^ " : " ^ pp_type t

  let pp_pattern pat =
    pp_name pat.matched.c_name ^ " binding " ^
    pp_lst ?sep:(Some " ") (fun s->s)
      (List.map (fun b_o -> match b_o with
           | None -> "_"
           | Some b -> pp_name b) pat.p_binds)
  
  let rec pp_assignment a = pp_binding a.lhs ^ " = " ^ pp_expr a.rhs
  and pp_expr e =
    match e with
    | Lit i -> string_of_int i
    | Var n -> pp_name n
    | App (f, aps) ->
      "( "
      ^ pp_expr f ^ " "
      ^ pp_lst pp_expr aps
      ^ ")"
    | Fun (binds, body) ->
      inc_indent ();
      let ppb = pp_lst pp_binding binds in
      let ppi = pp_expr body in
      dec_indent ();
      "λ" ^ ppb ^ ". " ^ ppi
    | Let (assigns, body) ->
      inc_indent ();
      let ppb = pp_lst pp_assignment assigns in
      let ppi = pp_expr body in
      dec_indent ();
      "let (" ^ ppb ^ ")\n.\n" ^ ppi
    | Dec (dec, pats, default) ->
      "case " ^ pp_expr dec ^ " of " ^
      pp_lst ?sep:(Some("\n")) (fun (pat,arm) ->
          pp_pattern pat ^
          " -> " ^
          pp_expr arm
        ) pats ^
      "\ndefault " ^
      match default with
      | None -> "_"
      | Some e -> pp_expr e
  ;;

  let rec pp_top_level tl = match tl with
    | TL_an at -> pp_assignment at
    | TL_ex ex -> pp_expr ex
    | TL_m m -> pp_mod m
    | TL_dt t -> pp_type t
  and pp_mod m =
    let open Util.OM in
    let n = m.mod_name >>| pp_name_atom |> Option.value ~default:"[Anonymous]" in
    let prefix = indent_line "{Module " ^ n ^ " :\nExpressions:\n" in
    let contents = pp_lst ~sep:"\n" pp_top_level m.top in
    prefix ^ contents ^ "}"
  ;;
end 
