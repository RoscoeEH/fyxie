(*
 * The idea is that the AST should contain all the structural info,
 * but while building it from the cst, we do arity and type checking,
 * and associate variable references to slots in binding scopes.
 * *)


(* TODO redo these / all of this with arrays instead of lists *)

(* TODO this is complicated by wanting to have it all work in a single
 * pass. That might not be the move, but I do want to avoid getting
 * into the mindset of a 20 pass compiler where we pass over the whole
 * tree every time I want to do things, since it's miserable for cache
 * coherency and frankly a kinda boring way to design a program. *)

open Result
open List

type name = string

type type_t =
| Int_t
| Fun_t of type_t * type_t

type binding = {

    name : name;
    tp : type_t;
  }

type v_ref = {
    name : name;
    tp : type_t;
    scope_depth : int;             (* 0 for closest scope *)
    slot : int;
  }

(* TODO make this a better alg. type. For instance move the type to
the top level so it's uniform. *)
type expr =
| Fun of func
| Let of let_block
| Var of v_ref
| App of application
| Lit of literal
and func =
  {
    f_n_slots : int;
    arity : int;
    f_args : binding list;
    captures : binding list;
    f_body : expr
  }
and let_block =
  {
    l_n_slots : int;
    binds : binding list;
    l_body : expr
  }
and application =
  {
    func : expr;
    a_args : expr list
  }
and literal =
  {
    tp : type_t;
    value : int
  }

(* eliminated by fixing the the lack or type field in expr *)
let rec type_of expr = match expr with
      | Lit v -> v.tp
      | Var v -> v.tp
      | Let l -> type_of l.l_body
      | Fun f ->
        let tb = type_of f.f_body in
        let build_type = fun ((_ as a) : binding) t -> Fun_t ((a.tp), t) in
        fold_right build_type f.f_args tb
      | App a ->
        (match a.func with
           | Fun f -> type_of f.f_body
           | _ -> raise (Failure "Application on non function typed object"))

(* simple lifts from cst to ast types *)
let from_cst_name = fun a -> a
let rec from_cst_type t = match t with
  | Cst.Int_t -> Int_t
  | Cst.Fun_t (hd, tl) -> Fun_t ((from_cst_type hd), (from_cst_type tl))
let from_cst_binding ((n, t) : Cst.binding) = { name = n ; tp = from_cst_type t; }

(* horrible runtime, but gathers the names that aren't shadowed in the expr *)
let rec names (scopes : Cst.binding list list) expr =
  let previously_bound scopes name = List.fold_left (fun present (n, _) -> present || (n == name)) false (concat scopes) in
  match expr with
      | Cst.Var name ->
          if previously_bound scopes name then [] else [name]
      | Cst.Let (binds, body) -> names (binds :: scopes) body
  | Cst.Fun (args, body) -> names (args :: scopes) body
  | Cst.App (func, args) -> concat_map (names scopes) (func :: args)
  | Cst.Lit _ -> []

let matching_binding n (scope : binding list) =
  let rec matching_binding1 n (scope : binding list) idx = match scope with
       | [] -> Error ()
      | b :: tail -> if n == b.name then Ok (b, idx) else matching_binding1 n tail (idx+1)
  in matching_binding1 n scope 0

(* fetch a binding from a scope, and return it along with some indexing info *)
let lookup_name name (scopes : binding list list) =
  let rec lookup_name1 name (scopes : binding list list) scope_idx = match scopes with
      | [] -> Error ()
      | s :: s_tail ->
          match matching_binding name s with
          | Error _ -> lookup_name1 name s_tail (scope_idx+1)
          | Ok (b, slot_idx) -> Ok (b, scope_idx, slot_idx) in
  lookup_name1 name scopes 0

(* Collects Ok values, stops on an Error *)
let sequence lst =
  let rec helper ok_vals remaining = match remaining with
    | [] -> Ok (List.rev ok_vals)
    | Error e :: _ -> Error e
    | Ok x :: xs -> helper (x :: ok_vals) xs
  in
  helper [] lst

(* TODO this is a huge mess. like 95% of the complexity is
 * here. Basically this takes a CST expr and makes an AST expr out of
 * it, but it also does type and arity checking. In addition, in order
 * to populate the interlinking data of the AST fields (like the var
 * reference index info), it also tracks a list of free variables that
 * get plumbed through as an output to sequence info.
 *
 * this screams to be fixed, and would also be a great candidate for a
 * monad of some sort, since it's basically about sequencing /
 * plumbing values over a compuatation tree.
 *
 * At a high level if you are using this in a non recursive way, it
 * should probably return an empty list of free variables. *)
let rec from_cst (scopes : binding list list) (free_list : name list) (expr : Cst.expr) =
  match expr with
      | Cst.Lit v -> (free_list, (Lit { value = v; tp = Int_t }))
      | Cst.Let (binds, body) ->
        let lifted_binds = map from_cst_binding binds in
          let (f_list, lifted_body) =
            from_cst (lifted_binds :: scopes) free_list body in
          (f_list,
            (Let
              {
                l_body = lifted_body;
                binds = lifted_binds;
                l_n_slots = (List.length lifted_binds)
                }))
      | Cst.App (func, args) ->
        let (f_fl, lifted_func) = from_cst scopes free_list func in
          (match lifted_func with
           | Fun inner_lifted_func ->
             if List.length args != inner_lifted_func.arity
             then raise (Failure "TODO error function arity doesn't match application")
             else let (final_fl, lifted_args) =
                 check_app (type_of lifted_func) scopes f_fl args in
               (final_fl, App {func = lifted_func; a_args = lifted_args})
           | _ -> raise (Failure "Application with non-function object"))
      | Cst.Var name ->
        (match lookup_name (from_cst_name name) scopes with
           | Error _ -> raise (Failure "test")
           | Ok (b, scope_idx, slot_idx) ->
             let lifted_var =
               Var
                 {
                   slot = slot_idx;
                   scope_depth = scope_idx;
                   tp = (b.tp);
                   name = (b.name)
                   } in
               (free_list, lifted_var))
      | Cst.Fun (args, body) ->
        let len_and_lift = fun len bind -> ((len + 1), (from_cst_binding bind)) in
        let (n_args, lifted_args) = fold_left_map len_and_lift 0 args in
        let (n_f_list, lifted_body) = from_cst [lifted_args] [] body in
        let cap_list = map (fun n -> lookup_name n scopes) n_f_list in
        (match sequence cap_list with
           | Error _ -> raise (Failure
             "TODO error name wasn't capturable / not defined")
           | Ok captures ->
             let lifted_func = Fun {
               f_body = lifted_body;
               captures = map (fun (b, _i, _j) -> b) captures;
               f_args = lifted_args;
               arity = n_args;
               f_n_slots = (n_args + (List.length captures))
               } in
             (append free_list n_f_list, lifted_func))
and check_app f_type scopes free_list exprs = match exprs with
  | [] -> (free_list, [])
  | arg :: arg_tail -> (match f_type with
    | Fun_t (a_t, r_t) ->
      let (new_fl, lifted_arg) = from_cst scopes free_list arg in
      if a_t != (type_of lifted_arg)
        then raise (Failure "TODO types don't match in function application")
        else let (final_fl, lifted_arg_tail) = check_app r_t scopes new_fl arg_tail in
      (final_fl, lifted_arg :: lifted_arg_tail)
    | _ -> raise (Failure "TODO Too many arguments supplied for function type"))
