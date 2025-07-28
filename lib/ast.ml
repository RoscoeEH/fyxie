(*
   * The idea is that the AST should contain all the structural info,
 * but while building it from the cst, we do arity and type checking,
 * and associate variable references to slots in binding scopes.
*)

open Result
open List
open Array

(* Helper for distinguishing partion and total applications
   can be moved later*)
let double_array_for_all f a1 a2 =
  Array.length a1 = Array.length a2
  &&
  let rec loop i =
    if i = Array.length a1 then true else f a1.(i) a2.(i) && loop (i + 1)
  in
  loop 0
;;

type name = string

type type_t =
  | Int_t
  | Fun_t of type_t array * type_t

(* TODO consider merging v_ref and binding, so that every name+type
   also has a slot idx *)
type binding =
  { b_name : name
  ; b_tp : type_t
  }

and v_ref =
  { v_name : name
  ; v_slot : int
  }

and func =
  { f_args : binding Array.t
  ; captures : (binding * int) Array.t
  ; f_body : expr
  }

and let_block =
  { binds : (binding * expr) Array.t
  ; l_body : expr
  }

and application =
  { func : expr
  ; a_args : expr Array.t
    (* TODO we need to distinguish between partial and total
   * application, aka are all the required arguments present. We
   * can't do that currently, since a function that returns a
   * function is a valid type, but a partially applied function of
   * two arguments is also typed as a curried function.
   *
   * See:
https://www.cambridge.org/core/journals/journal-of-functional-programming/article/making-a-fast-curry-pushenter-vs-evalapply-for-higherorder-languages/02447DB613E94DC35ACDCB24DB39F085
   *
   * I don't think we want to support partial application at the
   * moment, since it requires complexity in the runtime I don't
   * want to deal with currently. The issue is that there isn't really
   * a good place to insert the check that each application is total
   * and not partial. This is mostly cause things like applying a
   * let block that returns a function to an arg is valid, so we have
   * the function type but not the number of arguments at the
   * application site.
   *
   * TODO we could adjust the types to be more expressive to distinguish
   * int -> int -> int and int -> (int -> int)
   * or we could find some place where we can check this property
   * and fail on partial applications. I'd like to do the second if
   * possible since in the longer term partial application is
   * something we will want, and it seems annoying to change it
   * just to change it back later.
   *
   * At the moment we are going to use something similar to the push
   * model.
    *)
  }

and literal = { value : int }

and content =
  | Fun of func
  | Let of let_block
  | Var of v_ref
  | App of application
  | Lit of literal

and expr =
  { tp : type_t
  ; inner : content
  }

let pp_name n = n
let pp_lit l = "{Literal " ^ string_of_int l.value ^ "}"

let rec pp_type t =
  match t with
  | Int_t -> "Int"
  | Fun_t (args, result) ->
    Array.fold_left (fun acc arg -> acc ^ pp_type arg ^ " ") "(" args
    ^ "-> " ^ pp_type result
    ^ ")"
;;

let pp_binding b = pp_name b.b_name ^ " : " ^ pp_type b.b_tp
let pp_vref v = "{" ^ pp_name v.v_name ^ " @ " ^ string_of_int v.v_slot ^ "}"

let rec pp_func f =
  Array.fold_left (fun acc b -> acc ^ pp_binding b ^ " ") "λ " f.f_args
  ^ Array.fold_left (fun acc (b, _i) -> acc ^ pp_binding b ^ " ") "[ " f.captures
  ^ "] . "
  ^ pp_expr f.f_body

and pp_let l =
  Array.fold_left
    (fun acc (b, v) -> acc ^ "  " ^ pp_binding b ^ " = " ^ pp_expr v ^ "\n")
    "let\n"
    l.binds
  ^ ". "
  ^ pp_expr l.l_body

and pp_app a =
  Array.fold_left (fun acc e -> acc ^ pp_expr e ^ " ") ("( " ^ pp_expr a.func ^ " ") a.a_args
  ^ ")"

and pp_expr e =
  let s =
    match e.inner with
    | Fun f -> pp_func f
    | Let l -> pp_let l
    | Var v -> pp_vref v
    | App a -> pp_app a
    | Lit l -> pp_lit l
  in
  "{" ^ s ^ " : " ^ pp_type e.tp ^ "}"
;;

(* simple lifts from cst to ast types *)
let from_cst_name a = a

let rec from_cst_type t =
  match t with
  | Cst.Int_t -> Int_t
  | Cst.Fun_t (args, ret) -> Fun_t (Array.map from_cst_type args, from_cst_type ret)
;;

let from_cst_binding ((n, t) : Cst.binding) = { b_name = n; b_tp = from_cst_type t }

let pp_scopes scopes =
  print_endline "Scopes list";
  let _ = 
    List.map (fun b ->
        let _ = Array.map (fun b -> print_endline @@ pp_binding b) b in
        print_endline "") scopes
  in ()
;;

let find_index pred (arr : 'a array) : int option =
  let len = Array.length arr in
  let rec loop i =
    if i = len then None else if pred arr.(i) then Some i else loop (i + 1)
  in
  loop 0
;;

let matching_binding n (scope : binding array) =
  match find_index (fun b -> n = b.b_name) scope with
  | None -> None
  | Some idx -> Some (scope.(idx), idx)
;;

(* fetch a binding from a scope, and return it along with some indexing info *)
let lookup_name name (scopes : binding array list) =
  (*
   * print_endline @@ "lookup for " ^ name ^ " in: ";
   * pp_scopes scopes;
   *)
  let helper acc s =
    match acc with
    | Ok v -> Ok v
    | Error n ->
      (match matching_binding name s with
       | None -> Error (n + Array.length s)
       | Some (b, i) -> Ok (b, n + i))
  in
  let r = List.fold_left helper (Error 0) scopes in
  map_error (fun _ -> name ^ " not defined") r
;;

let collect_captures args body =
  let captures = ref [] in
  let rec cc1 ignores expr =
    match expr with
    | Cst.Lit _ -> ()
    | Cst.Var name ->
      if List.mem name ignores
      then ()
      else captures := name :: !captures
    | Cst.Let (binds, inner) ->
      let shadowed_names = List.map (fun ((b, _tp), _arm) -> b) binds in
      cc1 (List.append shadowed_names ignores) inner
    | Cst.App (func, args) ->
      cc1 ignores func;
      let _ = List.map (cc1 ignores) args in ()
    | Cst.Fun (binds, inner) ->
      let shadowed_names = List.map (fun (b, _tp) -> b) binds in
      cc1 (List.append shadowed_names ignores) inner
  in
  cc1 args body;
  let o = List.sort_uniq String.compare !captures in
  (*
   * print_string "Captured names from: ";
   * print_endline @@ Cst.pp_expr body;
   * let _ = List.map (fun n -> print_endline @@ "  " ^ n) o in
   * print_endline "";
   *)
  o
;;


(* TODO this is still not great but it it is better.
 *
 * It should probably return a result rather than panic.
 *
 * All the threading of the free_list makes a monad tempting but I'm
 * not sure how exactly to set it up tbh.
*)
let rec from_cst (scopes : binding array list) (free_list : name list) (expr : Cst.expr) =
  match expr with
  | Cst.Lit v -> free_list, { tp = Int_t; inner = Lit { value = v } }
  | Cst.Let (binds, body) ->
    let a_binds = of_list binds in
    let arms = Array.map (fun (_bind, expr) -> expr) a_binds in
    let binds' = Array.map (fun (b, _expr) -> from_cst_binding b) a_binds in
    (*
     * print_endline "let with scopes:";
     * pp_scopes (binds' :: scopes);
     *)
    let fl2, body' = from_cst (binds' :: scopes) free_list body in
    let fl3, arms' = lift_arms scopes fl2 arms in
    ( fl3
    , { tp = body'.tp
      ; inner = Let { l_body = body'; binds = Array.combine binds' arms' }
      } )
  | Cst.App (func, args) ->
    (*
     * print_endline "app with scope";
     * pp_scopes scopes;
     *)
    let fl2, func' = from_cst scopes free_list func in
    let fl3, args', r_t = check_app scopes fl2 args func'.tp in
    let a_t = Fun_t (Array.map (fun a -> a.tp) args', r_t) in
    fl3, { tp = a_t; inner = App { func = func'; a_args = args' } }
  | Cst.Var name ->
    let bind, slot_idx =
      match lookup_name (from_cst_name name) scopes with
      | Error s -> raise (Failure s)
      | Ok v -> v
    in
    free_list, { tp = bind.b_tp; inner = Var { v_name = name; v_slot = slot_idx } }
  | Cst.Fun (args, body) ->
    let args' = Array.map from_cst_binding (Array.of_list args) in
    let cap_names = collect_captures (List.map fst args) body in
    (*
     * print_endline "in func before lookup";
     *)
    let cap_sources = Util.sequence @@ List.map (fun n -> lookup_name n scopes) cap_names in
    match cap_sources with
    | Error e ->
      (*
       * print_endline "failed in the cap lookups";
       * let _ = List.map print_endline cap_names in
       *)
      raise @@ Failure e
    | Ok cap_sources ->
      let func_scopes = (Array.of_list @@ List.map fst cap_sources) :: args' :: [] in
      let fl2, body' = from_cst func_scopes [] body in
      let func' =
        Fun { f_args = args'; f_body = body'; captures = Array.of_list cap_sources }
      in
      let func_t = Fun_t (Array.map (fun b -> b.b_tp) args', body'.tp) in
      List.append free_list fl2, { tp = func_t; inner = func' }

and check_app scopes fl args f_t =
  (* checks arity + types, returns (freelist, lifted arg array, output type) or error*)
  let rec lift fl lst =
    match lst with
    | [] -> fl, []
    | hd :: tl ->
      let fl2, hd' = from_cst scopes fl hd in
      let fl3, tl' = lift fl2 tl in
      fl3, hd' :: tl'
  in
  let fl2, args' = (fun (fl, ag) -> fl, of_list ag) (lift fl args) in
  match f_t with
  | Fun_t (fa_t, r_t) ->
    let arg_types = Array.map (fun e -> e.tp) args' in
    let expected_arity = Array.length fa_t in
    let actual_arity = Array.length arg_types in
    if expected_arity <> actual_arity
    then
      let arg_types = Array.map (fun e -> e.tp) args' in
      if fa_t = arg_types
      then fl2, args', r_t
      else
        let expected_types = Array.fold_left (fun acc t -> acc ^ pp_type t ^ " ") "" arg_types in
        let got_types = Array.fold_left (fun acc t -> acc ^ pp_type t ^ " ") "" fa_t in
        let msg = Printf.sprintf "Argument type doesn't match in function application expected %sreceived %s"
            expected_types got_types in
        raise (Failure msg)
    else raise (Failure "Wrong number of arguments in function application")
  | _ -> raise (Failure "Application applied to non function type")

and lift_arms scopes fl arms =
  let helper fl2 arm = from_cst scopes fl2 arm in
  Array.fold_left_map helper fl arms
;;
