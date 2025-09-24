
open Option
open Result
open List
open Array

open Name
open Types

type type_t = Types.ty         (* limit to kind star *)

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

type pattern =
  { dtype : name
  ; matched : constructor
  ; p_binds : variable option array
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

and literal =
  { value : int
  } 

and case =
  { deconstruct : expr
  ; patterns : (pattern * expr) array
  ; default : expr option
  }

and content =
  | Fun of func
  | Let of let_block
  | Var of variable
  | App of application
  | Lit of literal
  | Dec of case

and expr =
  { tp : type_t
  ; inner : content
  } 

type top_level =
  | TL_an of assignment
  | TL_ex of expr
  | TL_m of mod_t
  | TL_dt of ty

and mod_t =
  { mod_name : name_atom option
  ; top : top_level Array.t
  } 

let mod_assigns m =
  List.filter_map (fun tl ->
      match tl with
      | TL_an at -> some at
      | _ -> none) @@ to_list m.top
;;

let mod_submods m =
  List.filter_map (fun tl ->
      match tl with
      | TL_m m -> some m
      | _ -> none) @@ to_list m.top
;;

module PrettyPrint = struct
  open Util.Pretty
         
  let pp_type = Types.pp_type
            
  let pp_lit l = "{Literal " ^ string_of_int l.value ^ "}"

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
    ^ " @ " ^ string_of_int v.v_id
    ^ " " ^ pp_domain v.v_domain
    ^ "}"
  ;;

  let pp_pattern p =
    let helper (v,t) =
      match v with
      | None -> ""
      | Some (v) ->
        pp_var v ^ " " ^ pp_type t
    in
    let z = Array.combine p.p_binds p.matched.c_args in
    pp_constructor p.matched ^
    (pp_arr helper z)
  
  let rec pp_assignment a =
    pp_var a.lhs ^ " = " ^ pp_expr a.rhs
  and pp_func f =
    "λ "
    ^ pp_arr pp_var f.f_args
    ^ "[ "
    ^ pp_arr pp_var f.captures
    ^ "] . "
    ^ pp_expr f.f_body
  and pp_let l =
    "let (\n"
    ^ pp_arr ?sep:(Some "\n") pp_assignment l.binds
    ^ ") .\n"
    ^ pp_expr l.l_body
  and pp_app a =
    Array.fold_left (fun acc e -> acc ^ pp_expr e ^ " ") ("( " ^ pp_expr a.func ^ " ") a.a_args
    ^ ")"
  and pp_dec c =
    let arm (p,e) = pp_pattern p ^ " -> " ^ pp_expr e in
    "case " ^ pp_expr c.deconstruct ^ " of " ^
    (pp_arr ?sep:(Some "\n") arm c.patterns) 
  and pp_expr e =
    let s =
      match e.inner with
      | Fun f -> pp_func f
      | Let l -> pp_let l
      | Var v -> pp_var v
      | App a -> pp_app a
      | Lit l -> pp_lit l
      | Dec c -> pp_dec c
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

  let rec pp_top_level tl = match tl with
    | TL_an a -> pp_assignment a
    | TL_ex e -> pp_expr e
    | TL_m m -> pp_mod m
    | TL_dt d -> pp_type d
    (*
     * | _ -> raise @@ Failure "top level td not supported"
     *)

  and pp_mod m =
    let open Util.OM in
    let open Util.Pretty in
    "Mod "
    ^ (m.mod_name >>| pp_name_atom |> Option.value ~default:"[Anonymous]")
    ^ " "
    ^ pp_arr ?sep:(Some "\n") pp_top_level m.top
  ;;
    
end
(*
 * open PrettyPrint
 *)

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
    | Ast.Dec (decon, cases, default) ->
      cc1 ignores decon;
      let _ = Option.map (cc1 ignores) default in
      let _ = cases |> List.map (fun ((pat : Ast.pattern),arm) ->
          let names = List.concat_map (fun n_opt ->
              Option.to_list n_opt 
            ) pat.p_binds
          in
          cc1 (names @ ignores) arm
        )
      in
      ()
  in
  cc1 args body;
  !captures
;;

let rec mark_as_captured (targets : (variable * int) list) body =
  let remove lst t = remove_assoc t lst in
  match body.inner with
  | Lit _ -> body
  | Fun _ -> body               (* wouldn't hurt but unnecessary *)
  | App app ->
    let f' = mark_as_captured targets app.func in
    let as' = Array.map (mark_as_captured targets) app.a_args in
    {tp=body.tp; inner=App ({func=f'; a_args=as'})}
  | Let lb ->
    let arms2 = Array.map (fun b -> mark_as_captured targets b.rhs) lb.binds in
    let bind_vs = Array.map (fun b -> b.lhs) lb.binds in
    let targets2 = Array.fold_left remove targets bind_vs in
    let body2 = mark_as_captured targets2 lb.l_body in
    { tp=body.tp
    ; inner=Let { l_body=body2
                ; binds=Array.map2 (fun b a -> {lhs=b; rhs=a}) bind_vs arms2}}
  | Var v ->
    if List.mem_assoc v targets && v.v_domain <> Static
    then { tp=body.tp
         ; inner=Var { v_name=v.v_name
                     ; v_tp=v.v_tp
                     ; v_domain=Closure (* the point of this function *)
                     ; v_id=v.v_id }}
    else { tp=body.tp; inner=Var v}
  | Dec case ->
    let dec = mark_as_captured targets case.deconstruct in
    let arm_helper (pat, body) =
      let t2 = Array.fold_left (fun lst t_opt -> match t_opt with
          | None -> lst
          | Some t -> remove lst t) targets pat.p_binds
      in
      (pat, mark_as_captured t2 body)
    in
    let pats = Array.map arm_helper case.patterns in
    let default = Option.map (mark_as_captured targets) case.default in
    { tp = body.tp
    ; inner =
        Dec { deconstruct = dec
            ; patterns = pats
            ; default = default
            }
    }
;;

let rec refs_allow_static ?(ignore=[]) body =
  (* To be called on an expression when attempting to compile a top level assignment *)
  match body.inner with
  | Lit _ -> true
  | Fun f -> refs_allow_static ~ignore f.f_body
  | Let l ->
    refs_allow_static ~ignore:((Array.map (fun b->b.lhs) l.binds |> Array.to_list) @ ignore) l.l_body
  | Dec c ->
    Array.for_all (fun (pat, arm) ->
        let ig2 = Array.to_list pat.p_binds |>
                  List.concat_map (Option.to_list) |>
                  fun additions -> additions @ ignore
        in
        refs_allow_static ~ignore:ig2 arm
      ) c.patterns
  | Var v -> 
    List.mem v ignore || v.v_domain = Arg || v.v_domain = Static
  | App a ->
    refs_allow_static ~ignore a.func && Array.for_all (refs_allow_static ~ignore) a.a_args
;;

let from_ast_type t = t ;;

(*
 * let from_ast_type_def ?(defs=[]) (td : Ast.type_def) =
 *  {lhs_t=td.lhs_t; rhs_t=from_ast_type ~defs td.rhs_t}
 * ;;
 *)

type ctx =
  { mutable statics         : variable list
  ; mutable static_next_id  : int
  ; mutable locals          : variable list
  ; mutable local_next_id   : int
  ; mutable args            : variable list
  ; mutable arg_next_id     : int
  ; mutable closures        : variable list
  ; mutable closure_next_id : int
  ; mutable current_mod     : name_atom list
  }

let empty_ctx () = { statics         = []
                   ; static_next_id  = 0
                   ; locals          = []
                   ; local_next_id   = 0 
                   ; args            = []
                   ; arg_next_id     = 0 
                   ; closures        = []
                   ; closure_next_id = 0
                   ; current_mod     = []
                   }

let save ctx =
  (* TODO surely there is a better way for a deep copy? *)
  { statics         = ctx.statics
  ; static_next_id  = ctx.static_next_id
  ; locals          = ctx.locals
  ; local_next_id   = ctx.local_next_id
  ; args            = ctx.args
  ; arg_next_id     = ctx.arg_next_id
  ; closures        = ctx.closures
  ; closure_next_id = ctx.closure_next_id
  ; current_mod     = ctx.current_mod
  }
;;

let restore ~dst ~src =
  (* TODO again this sucks *)
  dst.statics         <- src.statics        ;
  dst.static_next_id  <- src.static_next_id ;
  dst.locals          <- src.locals         ;
  dst.local_next_id   <- src.local_next_id  ;
  dst.args            <- src.args           ;
  dst.arg_next_id     <- src.arg_next_id    ;
  dst.closures        <- src.closures       ;
  dst.closure_next_id <- src.closure_next_id;
  dst.current_mod     <- src.current_mod
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

let make_name_abs ctx name =
  let helper p n =
    match add_prefix p n with
    | Ok x -> x
    | Error e -> raise @@ Failure e
  in
  List.fold_right helper ctx.current_mod name
;;

let insert ctx name tp domain =
  let id = next_id ctx domain in
  let n = match domain with
    | Static -> make_name_abs ctx name
    | _ -> name
  in
  let v = {v_name=n; v_tp=tp; v_domain=domain; v_id=id} in
  (match domain with
  | Static -> ctx.statics <- v :: ctx.statics
  | Local -> ctx.locals <- v :: ctx.locals
  | Arg -> ctx.args <- v :: ctx.args
  | Closure -> ctx.closures <- v :: ctx.closures);
  v
;;

let lookup ctx name =
  let open Util.OM in
  let abs_name = make_name_abs ctx name in
  let pred n = name = n.v_name || abs_name = n.v_name in
  let scopes = [ctx.locals; ctx.args; ctx.closures; ctx.statics] in
  let results = List.map (List.find_opt pred) scopes in
  let r = List.fold_left (<|>) none results in
  r 
;;

let rec from_ast_assign ~domain ~ctx (an : Ast.assignment) =
  (* Mutates ctx to include the assignment in the given domain *)
  let (n,tp) = an.lhs in
  let expr = an.rhs in
  let tp' = from_ast_type tp in
  let expr' = from_ast_expr ctx expr in
  match lookup ctx n with
  | None -> 
    let var = insert ctx n tp' domain in
    {lhs=var; rhs=expr'}
  | Some var ->
    {lhs=var; rhs=expr'}

and from_ast_expr ctx (expr : Ast.expr) = match expr with
  | Lit i -> {tp=Types.int_t; inner=Lit {value=i}}
  | App (func, args) ->
    let func' = from_ast_expr ctx func in
    let args' = List.map (from_ast_expr ctx) args in
    let rec helper f_tp args =
      match f_tp,args with
      | ret_t,[] -> ret_t
      | Ta (Ta (app, _), f_rest), _::a_rest -> if app == Types.arrow_t
        then helper f_rest a_rest
        else raise @@ Failure "Non-arrow type applied during function application"
      | _,_a::_a_rest -> raise @@ Failure "Too many arguments in application"
    in
    let ret_tp = helper func'.tp args' in
    {tp = ret_tp; inner= App {func=func'; a_args=Array.of_list args'}}
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
    let cap_vars = List.filter (fun v -> v.v_domain <> Static) cap_vars in
    let cap_vars_i = List.mapi (fun i a -> (a,i)) cap_vars in
    ctx.closures <- cap_vars;
    ctx.closure_next_id <- List.length cap_vars;

    ctx.locals <- [];
    ctx.local_next_id <- 0;
    let lift_arg (n,tp) =
      insert ctx n (from_ast_type tp) Arg
    in
    let args' = List.map lift_arg args in
    let body' = mark_as_captured cap_vars_i @@ from_ast_expr ctx body in
    let a_tps = Array.of_list @@ List.map (fun v -> v.v_tp) args' in
    let f_tp =
      Array.fold_right (fun t acc -> (Ta (Ta (Types.arrow_t, t), acc))) a_tps body'.tp
    in
    let _ = restore ~dst:ctx ~src:prior_ctx in
    { tp=f_tp
    ; inner=Fun { f_args=Array.of_list args'
                ; captures=Array.of_list cap_vars
                ; f_body=body'}}
  | Dec (deconstruct, cases, default) ->
    let lift_pattern (pat : Ast.pattern) =
      let types = pat.matched.c_args in
      let p_binds' = 
        if Array.length types < List.length pat.p_binds
        then raise @@ Failure ("Pattern doesn't match number of types in constructor " ^ pp_name pat.matched.c_name)
        else
          let name_type_to_var name c_type =
            Option.map (fun n -> insert ctx n c_type Local) name
          in
          Array.map2 name_type_to_var (Array.of_list pat.p_binds) types
      in
      { dtype = pat.dtype
      ; matched = pat.matched
      ; p_binds = p_binds' }
    in
    let dec' = from_ast_expr ctx deconstruct in
    let arms' = cases |> List.map (fun (pat,arm) ->
        let prior = save ctx in
        let pat' = lift_pattern pat in
        let arm' = from_ast_expr ctx arm in
        let () = restore ~dst:ctx ~src:prior in
        (pat',arm')
      ) |> Array.of_list
    in
    let default' = Option.map (from_ast_expr ctx) default in
    let tp =
      let pt = if Array.length arms' = 0
        then None
        else Some ((snd (Array.get arms' 0)).tp)
      in
      match pt,(Option.map (fun x -> x.tp) default') with
      | None,None -> raise @@ Failure "Empty case statement?"
      | Some tp, _ | None, Some tp -> tp
    in
    let inner =
      Dec { deconstruct = dec'
          ; patterns = arms'
          ; default = default'
          }
    in
    { tp = tp
    ; inner = inner
    }
;;

let tl_assign ~domain ~ctx (an : Ast.assignment) =
  (* Mutates ctx to define the name in the given domain *)
  let (n,tp) = an.lhs in
  let tp' = from_ast_type tp in
  let _ = insert ctx n tp' domain in
  ()
;;

let step_down_cur_mod ctx (m:Ast.mod_t) =
  let open Util.OM in
  let n_mod cm mn = mn >>| (fun x -> cm @ [x]) |> Option.value ~default:cm in
  ctx.current_mod <- n_mod ctx.current_mod m.mod_name
;;

let declare_ahead ctx (m : Ast.mod_t) =
  let rec helper tls =
      List.map (fun x -> match x with
          | Ast.TL_an a -> tl_assign ~domain:Static ~ctx a
          | Ast.TL_m sm ->
            let p_mod = ctx.current_mod in
            step_down_cur_mod ctx sm;
            let _ = helper sm.top in 
            ctx.current_mod <- p_mod;
            ()
          | _ -> ()
        ) tls      
  in
  let p_mod = ctx.current_mod in
  step_down_cur_mod ctx m;
  let _ = helper m.top in
  ctx.current_mod <- p_mod;
  ()
;;

let rec from_ast_top_level ctx tl = match tl with
  | Ast.TL_an an ->
    let an' = from_ast_assign ~domain:Static ~ctx an in
    TL_an an'
  | Ast.TL_ex expr ->
    let expr' = from_ast_expr ctx expr in
    TL_ex expr'
  | Ast.TL_m m ->
    let m' = from_ast_mod ctx m in
    TL_m m'
  | Ast.TL_dt d ->
    (* TODO ? ensure anything here *)
    TL_dt d

and from_ast_mod ctx (m : Ast.mod_t) =
  let p_mod = ctx.current_mod in
  step_down_cur_mod ctx m;
  let tl' = List.map (from_ast_top_level ctx) m.top in
  let out = 
    { mod_name = m.mod_name
    ; top=Array.of_list tl'
    }
  in
  ctx.current_mod <- p_mod;
  out
;;    
