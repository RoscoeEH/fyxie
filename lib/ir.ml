
open Option
open Result
open List
open Array

open Name

type type_t =
  | Int_t
  | Fun_t of type_t array * type_t
  | Alias_t of name

type type_def =
  { lhs_t : name
  ; rhs_t : type_t
  }

type domain =
  | Static
  | Local
  | Closure
  | Arg

type variable =
  { v_name : name
  ; v_tp : type_t
  ; v_domain : domain
  ; v_id : int
  }

type assignment =
  { lhs : variable
  ; rhs : expr
  }

and func =
  { f_args : variable Array.t
  ; captures : variable Array.t
  ; f_body : expr
  }

and let_block =
  { binds : assignment Array.t
  ; l_body : expr
  }

and application =
  { func : expr
  ; a_args : expr Array.t
  }

and literal = { value : int }

and content =
  | Fun of func
  | Let of let_block
  | Var of variable
  | App of application
  | Lit of literal

and expr =
  { tp : type_t
  ; inner : content
  }

type top_level =
  | TL_td of type_def
  | TL_an of assignment
  | TL_ex of expr

type mod_t =
  { mod_name : name option
  ; top : top_level Array.t
  }

let mod_types m =
  List.filter_map (fun tl ->
      match tl with
      | TL_td td -> some td
      | _ -> none) @@ to_list m.top
;;

let mod_assigns m =
  List.filter_map (fun tl ->
      match tl with
      | TL_an at -> some at
      | _ -> none) @@ to_list m.top
;;

let fetch_alias s defs = List.find_opt (fun td -> td.lhs_t = s) defs

let rec fetch_nearest_alias s ms =
  match ms with
  | [] -> None
  | m::rest -> (match fetch_alias s @@ mod_types m with
    | Some v -> Some v
    | None -> fetch_nearest_alias s rest)
;;

module PrettyPrint = struct
  let pp_lit l = "{Literal " ^ string_of_int l.value ^ "}"

  let rec pp_type ?(mods=[]) t =
    match t with
    | Int_t -> "Int"
    | Fun_t (args, result) ->
      Array.fold_left (fun acc arg -> acc ^ pp_type arg ^ " ") "(" args
      ^ "-> " ^ pp_type result
      ^ ")"
    | Alias_t s ->
      let b = fetch_nearest_alias s mods in
      let prefix = "{ Alias : " ^ pp_name s ^ " bound to " in
      (match b with
       | None -> prefix ^ "nothing }"
       | Some td -> prefix ^ pp_type ~mods:mods td.rhs_t ^ "}")
  ;;

  let pp_domain d = match d with
    | Static -> "static"
    | Local -> "local"
    | Closure -> "closure"
    | Arg -> "arg"
  ;;

  let pp_var v =
    "{"
    ^ pp_name v.v_name
    ^ " : " ^ pp_type v.v_tp
    ^ " in " ^ pp_domain v.v_domain
    ^ " @ " ^ string_of_int v.v_id
    ^ "}"
  ;;

  let rec pp_assignment a =
    pp_var a.lhs ^ " = " ^ pp_expr a.rhs
  and pp_func f =
    Array.fold_left (fun acc b -> acc ^ pp_var b ^ " ") "λ " f.f_args
    ^ Array.fold_left (fun acc b -> acc ^ pp_var b ^ " ") "[ " f.captures
    ^ "] . "
    ^ pp_expr f.f_body
  and pp_let l =
    Array.fold_left
      (fun acc a -> acc ^ pp_assignment a ^ "\n")
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
      | Var v -> pp_var v
      | App a -> pp_app a
      | Lit l -> pp_lit l
    in
    "{" ^ s ^ " : " ^ pp_type e.tp ^ "}"
  ;;

  let pp_scopes scopes =
    print_endline "Scopes list";
    let _ =
      List.map (fun b ->
          let _ = Array.map (fun b -> print_endline @@ pp_var b) b in
          print_endline "") scopes
    in ()
  ;;
end
open PrettyPrint

let unify_types ?(defs = []) a b =
  (* just a better eq check atm, more later *)
  let open Util.RM in
  let rec ut1 a b =
    match (a,b) with
    | (Int_t, Int_t) -> ok a
    | (Fun_t (aa, ar), Fun_t (ba, br)) ->
      let arg_ms' = Array.map2 ut1 aa ba in
      (let* args' = Util.sequence_arr arg_ms' in
       let* r' = ut1 ar br in
       return @@ Fun_t (args', r'))
      |> Result.map_error (fun e ->
          "When unifying "
          ^ pp_type a
          ^ "\nand\n"
          ^ pp_type b
          ^ "\nencountered\n"
          ^ e)
    | (Alias_t an, Alias_t bn) ->
      (match fetch_alias an defs with
       | None -> error (pp_name an ^ " Undefined")
       | Some a' -> (match fetch_alias bn defs with
           | None -> error (pp_name bn ^ " Undefined")
           | Some b' -> ut1 a'.rhs_t b'.rhs_t))
    | (_,_) ->
      error ("Types\n"
             ^ pp_type a
             ^ "\nand\n"
             ^ pp_type b
             ^ "don't match")
  in
  ut1 a b
;;

let collect_captures args body =
  let captures = ref [] in
  let rec cc1 ignores expr =
    match expr with
    | Ast.Lit _ -> ()
    | Ast.Var name ->
      if List.mem name ignores
      then ()
      else captures := name :: !captures
    | Ast.Let (binds, inner) ->
      let shadowed_names = List.map (fun (an:Ast.assignment) -> fst an.lhs) binds in
      cc1 (List.append shadowed_names ignores) inner
    | Ast.App (func, args) ->
      cc1 ignores func;
      let _ = List.map (cc1 ignores) args in ()
    | Ast.Fun (binds, inner) ->
      let shadowed_names = List.map (fun (b, _tp) -> b) binds in
      cc1 (List.append shadowed_names ignores) inner
  in
  cc1 args body;
  !captures
;;

let rec from_ast_type ?(defs=[]) at =
  match at with
  | Ast.Int_t -> Int_t
  | Ast.Fun_t (args, ret) ->
    let args' = Array.map (fun a -> from_ast_type ~defs a) args in
    Fun_t (args', from_ast_type ~defs ret)
  | Ast.Alias_t n ->
    match List.find_opt (fun td -> td.lhs_t = n) defs with
    | Some td -> td.rhs_t
    | None -> raise @@ Failure ("Type alias " ^ pp_name n ^ " not defined")
;;

let from_ast_type_def ?(defs=[]) (td : Ast.type_def) =
 {lhs_t=td.lhs_t; rhs_t=from_ast_type ~defs td.rhs_t}
;;

type ctx =
  { mutable defs            : type_def list
  ; mutable statics         : variable list
  ; mutable static_next_id  : int
  ; mutable locals          : variable list
  ; mutable local_next_id   : int
  ; mutable args            : variable list
  ; mutable arg_next_id     : int
  ; mutable closures        : variable list
  ; mutable closure_next_id : int
  }

let empty_ctx () = { defs            = []
                   ; statics         = []
                   ; static_next_id  = 0
                   ; locals          = []
                   ; local_next_id   = 0 
                   ; args            = []
                   ; arg_next_id     = 0 
                   ; closures        = []
                   ; closure_next_id = 0 
                   }

let save ctx =
  (* TODO surely there is a better way for a deep copy? *)
  { defs            = ctx.defs
  ; statics         = ctx.statics
  ; static_next_id  = ctx.static_next_id
  ; locals          = ctx.locals
  ; local_next_id   = ctx.local_next_id
  ; args            = ctx.args
  ; arg_next_id     = ctx.arg_next_id
  ; closures        = ctx.closures
  ; closure_next_id = ctx.closure_next_id
  }
;;

let restore ~dst ~src =
  (* TODO again this sucks *)
  dst.defs            <- src.defs           ;
  dst.statics         <- src.statics        ;
  dst.static_next_id  <- src.static_next_id ;
  dst.locals          <- src.locals         ;
  dst.local_next_id   <- src.local_next_id  ;
  dst.args            <- src.args           ;
  dst.arg_next_id     <- src.arg_next_id    ;
  dst.closures        <- src.closures       ;
  dst.closure_next_id <- src.closure_next_id
;;

let next_id ctx domain = match domain with
  | Arg ->
    ctx.arg_next_id <- ctx.arg_next_id+1;
    ctx.arg_next_id-1
  | Static ->
    ctx.static_next_id <- ctx.static_next_id+1;
    ctx.static_next_id-1
  | Local ->
    ctx.local_next_id <- ctx.local_next_id+1;
    ctx.local_next_id-1
  | Closure ->
    ctx.closure_next_id <- ctx.closure_next_id+1;
    ctx.closure_next_id-1
;;

let insert ctx name tp domain =
  let id = next_id ctx domain in
  let v = {v_name=name; v_tp=tp; v_domain=domain; v_id=id} in
  (match domain with
  | Static -> ctx.statics <- v :: ctx.statics
  | Local -> ctx.locals <- v :: ctx.locals
  | Arg -> ctx.args <- v :: ctx.args
  | Closure -> ctx.closures <- v :: ctx.closures);
  v
;;

let lookup ctx name =
  let pred n = name = n.v_name in
  let scopes = [ctx.locals; ctx.closures; ctx.statics] in
  let results = List.map (List.find_opt pred) scopes in
  let r = List.fold_left (fun a b -> if is_some a then a else b) none results in
  r
;;

let rec from_ast_assign ~domain ~ctx (an : Ast.assignment) =
  (* Mutates ctx to include the assignment in the given domain *)
  let (n,tp) = an.lhs in
  let expr = an.rhs in
  let tp' = from_ast_type ~defs:ctx.defs tp in
  let expr' = from_ast_expr ctx expr in
  let var = insert ctx n tp' domain in
  match unify_types ~defs:ctx.defs expr'.tp tp' with
  | Error e -> raise @@ Failure e
  | Ok _u_tp' -> {lhs=var; rhs=expr'}

and from_ast_expr ctx (expr : Ast.expr) = match expr with
  | Lit i -> {tp=Int_t; inner=Lit {value=i}}
  | App (func, args) ->
    let func' = from_ast_expr ctx func in
    let args' = List.map (from_ast_expr ctx) args in
    let (fa_tp, ret_tp) = match func'.tp with
      | Fun_t (i, o) -> (i, o)
      | _ -> raise @@
        Failure ("Non-func type in application: " ^ pp_type func'.tp)
    in
    let a_tps = Array.of_list @@ List.map (fun a -> a.tp) args' in
    (match Util.sequence_arr
             (Array.map2 (unify_types ~defs:ctx.defs) fa_tp a_tps)
     with
     | Ok _arg_tps ->
       {tp = ret_tp; inner= App {func=func'; a_args=Array.of_list args'}}
     | Error e ->
       raise @@ Failure (
         "When applying function\n"
         ^ pp_expr func'
         ^ "\ntype error:\n"
         ^ e
         ^ " Occured"))
  | Var n -> (match lookup ctx n with
      | Some v -> {tp=v.v_tp; inner=Var v}
      | None -> raise @@ Failure ("Name " ^ pp_name n ^ "not defined"))
  | Let (binds, body) ->
    let prior_ctx = save ctx in
    let vars = List.map (from_ast_assign ~domain:Local ~ctx:ctx) binds in
    (* assign already extends ctx for us *)
    let body' = from_ast_expr ctx body in
    let out = {tp=body'.tp; inner=Let {binds=Array.of_list vars; l_body=body'}} in
    let _ = restore ~dst:ctx ~src:prior_ctx in
    (* don't let binds escape into the next thing we call from_ast_expr on *)
    out
  | Fun (args, body) ->
    let prior_ctx = save ctx in
    let cap_names = collect_captures (List.map fst args) body in
    let ensure_def n = match lookup ctx n with
      | None -> raise @@ Failure ("Name " ^ pp_name n ^ " Not defined")
      | Some v -> v
    in
    let cap_vars = List.map ensure_def cap_names in
    ctx.closures <- cap_vars;
    ctx.closure_next_id <- List.length cap_vars;

    ctx.locals <- [];
    ctx.local_next_id <- 0;
    let lift_arg (n,tp) =
      insert ctx n (from_ast_type ~defs:ctx.defs tp) Arg
    in
    let args' = List.map lift_arg args in
    let body' = from_ast_expr ctx body in
    let a_tps = Array.of_list @@ List.map (fun v -> v.v_tp) args' in
    let f_tp = Fun_t (a_tps, body'.tp) in
    let _ = restore ~dst:ctx ~src:prior_ctx in
    { tp=f_tp
    ; inner=Fun { f_args=Array.of_list args'
                ; captures=Array.of_list cap_vars
                ; f_body=body'}}
;;

let from_ast_top_level ctx tl = match tl with
  | Ast.TL_td td ->
    let td' = from_ast_type_def ~defs:ctx.defs td in
    ctx.defs <- td' :: ctx.defs;
    TL_td td'
  | Ast.TL_an an ->
    let an' = from_ast_assign ~domain:Static ~ctx an in
    TL_an an'
  | Ast.TL_ex expr ->
    let expr' = from_ast_expr ctx expr in
    TL_ex expr'
;;

let from_ast_mod ctx (m : Ast.mod_t) =
  let tl' = List.map (from_ast_top_level ctx) m.top in
  {mod_name = m.mod_name; top=Array.of_list tl'}
;;    
