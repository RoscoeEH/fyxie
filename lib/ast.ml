(*
   Define basic structure as a recursive tree type
*)

open Util

(* Make AST handle type checking, closures, and var linking *)

(* TODO update this to something that looks more like a path *)
type name = string

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
  | Int_t -> print_endline "Int"
  | Fun_t (args, result) ->
    print_endline "(";
    Pretty.inc_indent ();
    Array.iter
      (fun arg ->
         pp_type ~mods arg;
         print_endline "->")
      args;
    pp_type ~mods result;
    Pretty.dec_indent ();
    print_endline ")"
  | Alias_t s ->
    let prefix = "{ Alias : " ^ s ^ " bound to" in
    print_endline prefix;
    (match fetch_nearest_alias s mods with
     | None -> print_endline "nothing"
     | Some td ->
       Pretty.inc_indent ();
       pp_type ~mods td.rhs_t;
       Pretty.dec_indent ();
       print_endline "}")
;;

let pp_name s = Pretty.print_endline s

let pp_type_def td =
  Pretty.print_endline (td.lhs_t ^ " :=");
  Pretty.inc_indent ();
  pp_type td.rhs_t;
  Pretty.dec_indent ()
;;

let pp_binding (n, t) =
  Pretty.print (n ^ " : ");
  pp_type t
;;

let rec pp_assignment a =
  pp_binding a.lhs;
  Pretty.print " = ";
  pp_expr a.rhs;
  Pretty.print_endline ""

and pp_expr e =
  match e with
  | Lit i -> Pretty.print_endline (string_of_int i)
  | Var n -> Pretty.print_endline n
  | App (f, aps) ->
    Pretty.print_endline "(";
    Pretty.inc_indent ();
    pp_expr f;
    List.iter pp_expr aps;
    Pretty.dec_indent ();
    Pretty.print_endline ")"
  | Fun (binds, body) ->
    Pretty.print "λ ";
    List.iter
      (fun b ->
         pp_binding b;
         Pretty.print " ")
      binds;
    Pretty.print_endline ".";
    Pretty.inc_indent ();
    pp_expr body;
    Pretty.dec_indent ()
  | Let (assigns, body) ->
    Pretty.print_endline "let";
    Pretty.inc_indent ();
    List.iter pp_assignment assigns;
    Pretty.dec_indent ();
    Pretty.print_endline "in";
    pp_expr body
;;

let pp_mod m =
  Pretty.print_endline "{Module with types:";
  Pretty.inc_indent ();
  List.iter pp_type_def m.types;
  Pretty.dec_indent ();
  Pretty.print_endline "And assignments:";
  Pretty.inc_indent ();
  List.iter pp_assignment m.assigns;
  Pretty.dec_indent ();
  Pretty.print_endline "}"
;;
