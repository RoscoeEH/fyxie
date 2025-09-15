(* Define basic structure as a recursive tree type *)
open Option
    
open Name

type type_t = Types.Basic.ty    (* kind star *)

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

type top_level =
  | TL_td of type_def
  | TL_an of assignment
  | TL_ex of expr
  | TL_m  of mod_t
      
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
  
let mod_types m =
  List.filter_map (fun tl ->
      match tl with
      | TL_td td -> some td
      | _ -> none) m.top
;;

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

let fetch_alias s m = List.find_opt (fun td -> td.lhs_t = s) @@ mod_types m

let rec fetch_nearest_alias s ms =
  match ms with
  | [] -> None
  | m :: rest ->
    (match fetch_alias s m with
     | Some v -> Some v
     | None -> fetch_nearest_alias s rest)
;;

module PrettyPrint = struct
  open Util.Pretty
  let pp_type = Types.PrettyPrint.pp_type

  let pp_type_def td = pp_name td.lhs_t ^ " := " ^ pp_type td.rhs_t

  let pp_binding (n, t) = pp_name n ^ " : " ^ pp_type t

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
  ;;

  let rec pp_top_level tl = match tl with
    | TL_td td -> pp_type_def td
    | TL_an at -> pp_assignment at
    | TL_ex ex -> pp_expr ex
    | TL_m m -> pp_mod m
  and pp_mod m =
    let open Util.OM in
    let n = m.mod_name >>| pp_name_atom |> Option.value ~default:"[Anonymous]" in
    let prefix = indent_line "{Module " ^ n ^ " :\nExpressions:\n" in
    let exprs = pp_lst ~sep:"\n" pp_expr @@ mod_exprs m in
    let assigns = pp_lst ~sep:"\n" pp_assignment @@ mod_assigns m in
    let mods = pp_lst ~sep:"\n" pp_mod @@ mod_submods m in
    prefix ^ exprs ^ "Assignments:\n" ^ assigns ^ "Submodules:\n" ^ mods ^ "}"
  ;;
end 
