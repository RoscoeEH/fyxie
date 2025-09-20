
open Types.Builtins

open Ir

open Util.RM
type 'a r = ('a, string) Result.t

let fail_msg str = Error str

let rec unroll_ftp t =
  match as_function t with
  | Ok (a,b) ->
    unroll_ftp b >>| fun rs -> a::rs
  | Error _e ->
    return [t]


let roll_ftp tps = (* expects reversed list *)
  match tps with
  | [] -> raise @@ Failure "Can't roll empty list into function type"
  | a::rs ->
    List.fold_left (fun acc t ->
        mk_function t acc) a rs

let rec check_expr tbinds expr =
  match expr.inner with
  | Fun f ->
    let* caps' = Util.sequence_arr @@
      Array.map (fun v -> check_expr tbinds {tp=v.v_tp;inner=Var v}) f.captures in
    let caps' = Array.map
        (fun e ->
           match e.inner with
           | Var v -> v
           | _ -> raise @@ Failure "")
        caps' in
    let f_ext = Array.map (fun v -> (v.v_id, v.v_tp)) f.f_args in
    let c_ext = Array.map (fun v -> (v.v_id, v.v_tp)) caps' in
    let extension = Array.to_list @@ Array.append f_ext c_ext in
    let* body' = check_expr (extension @ tbinds) f.f_body in
    let func' = {f_args=f.f_args; captures=caps'; f_body=body'} in
    let* ret_tp = unroll_ftp expr.tp >>|
      List.drop (Array.length f.f_args) >>| List.rev >>|
      List.hd
    in
    let* ret_tp = unify ret_tp body'.tp in 
    let arg_tps = List.rev @@ Array.to_list @@ Array.map (fun v -> v.v_tp) f.f_args in
    let f_tp = roll_ftp @@ arg_tps @ [ret_tp] in
    return {tp=f_tp; inner=Fun func'}
  | App a ->
    let* func' = check_expr tbinds a.func in
    let* args' = Util.sequence_arr @@
      Array.map (check_expr tbinds) a.a_args in
    let* arg_types = unroll_ftp func'.tp >>| Array.of_list in
    let helper a tp =
      let* tp = unify a.tp tp in
      return {tp=tp; inner=a.inner}
    in
    let* args'' = Util.sequence_arr @@
      Array.map2 helper args' arg_types in
    let* tp =
      unroll_ftp func'.tp >>|
      List.drop (Array.length args') >>| List.rev >>|
      roll_ftp in
    return {tp=tp; inner=App {func=func'; a_args=args''}}
  | Let lb ->
    let* binds' = Util.sequence_arr @@
      Array.map (check_assignment tbinds) lb.binds in
    let extension = Array.to_list @@ Array.map
        (fun asn -> (asn.lhs.v_id, asn.lhs.v_tp))
        lb.binds in
    let* body' = check_expr (extension @ tbinds) lb.l_body in
    let lb' = {binds=binds'; l_body=body'} in
    return {tp=body'.tp; inner=Let lb'}
  | Var v ->
    begin match List.assoc_opt v.v_id tbinds with
      | Some def_t ->
        let* tp = unify v.v_tp def_t in
        return {tp=tp; inner=Var v}
      | None -> raise @@ Failure "Variable id not in type scope?"
    end
  | Lit _l ->
    let* tp = unify expr.tp int_t in
    return {tp=tp; inner=expr.inner}
and check_assignment tbinds assign =
  let* rhs' = check_expr tbinds assign.rhs in
  let lhs = assign.lhs in
  let* tp = unify assign.lhs.v_tp rhs'.tp in
  let lhs' = {v_name=lhs.v_name; v_tp=tp; v_domain=lhs.v_domain; v_id=lhs.v_id} in
  return {lhs=lhs'; rhs=rhs'}

let rec check_top_level tbinds tl =
  match tl with
  | TL_an an -> check_assignment tbinds an >>| fun an ->
    let b = (an.lhs.v_id, an.rhs.tp) in
    (b::tbinds, TL_an an)
  | TL_ex e -> check_expr tbinds e >>| fun x ->
    (tbinds, TL_ex x)
  | TL_m m -> check_mod tbinds m >>|
    fun (tbinds, x) -> (tbinds, TL_m x)
and check_mod tbinds m =
  let helper acc tl =
    let r = check_top_level acc tl in
    match r with
    | Error e -> (acc, Error e)
    | Ok (acc, r) -> (acc, Ok r)
  in
  let (tbinds, top'_r) = 
    Array.fold_left_map helper tbinds m.top in
  let* top' = Util.sequence_arr top'_r in
  return (tbinds, {mod_name=m.mod_name; top=top'})
