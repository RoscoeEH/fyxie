(*
   Define basic structure as a recursive tree type
*)

open Util

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
  | Int_t -> Pretty.print_endline "Int"
  | Fun_t (args, result) ->
    Pretty.print "(";
    Pretty.inc_indent ();
    Array.iter
      (fun arg ->
         pp_type arg;
         Pretty.print "-> ")
      args;
    pp_type result;
    Pretty.dec_indent ();
    Pretty.print_endline ")"
;;

let pp_name s = s

let pp_binding (n, t) =
  Pretty.print (n ^ " : ");
  pp_type t
;;

let rec pp_expr e =
  match e with
  | Lit i -> Pretty.print_endline (string_of_int i)
  | Var n -> Pretty.print_endline (pp_name n)
  | App (f, args) ->
    Pretty.print_endline "(";
    Pretty.inc_indent ();
    pp_expr f;
    List.iter pp_expr args;
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
  | Let (binds, body) ->
    Pretty.print_endline "let";
    Pretty.inc_indent ();
    List.iter
      (fun (b, v) ->
         pp_binding b;
         Pretty.print " = ";
         pp_expr v)
      binds;
    Pretty.dec_indent ();
    Pretty.print_endline ". ";
    Pretty.inc_indent ();
    pp_expr body;
    Pretty.dec_indent ()
;;
