(* https://web.cecs.pdx.edu/~mpj/thih/thih.pdf *)
open Name

(** Module for various helper functions that aren't specific to types.
 *
 * Mostly just stuff that haskell has in the standard libarary. *)
module Helpers = struct
  let union l1 l2 =
    let rec u1 acc l1 l2 =
      match l1 with
      | l::ls -> if List.mem l acc then u1 acc ls l2 else u1 (l::acc) ls l2
      | [] -> begin match l2 with
          | [] -> acc
          | r::rs -> if List.mem r acc then u1 acc [] rs else u1 (r::acc) [] rs
        end
    in List.rev @@ u1 [] l1 l2

  let intersect l1 l2 = List.filter (fun a -> List.mem a l2) l1
end
open Helpers


(** Module for the basic type details.
 *
 * Contains Types, Type variables, Kinds, Substiutions, and some basic helpers.
*)
module Basic = struct
  type kind =
    | Star
    | Kf of kind * kind

  type tvar =
    { tvar_name : name
    ; tvar_kind : kind
    }

  type ty =
    | Tv of tvar
    | Tc of name * kind
    | Ta of ty * ty
    | Tg of int

  let rec kind_of (ty : ty) = match ty with
    | Tv (v) -> v.tvar_kind
    | Tc (_, k) -> k
    | Ta (t, _) -> begin match kind_of t with
        | Kf (_,b) -> b
        | _ -> raise (Failure "Kind ill-formed type application")
      end
    | Tg _ -> raise (Failure "Kind of general tv?")

  type subst = (tvar * ty) list

  let null_s : subst = []

  let single_s n t = [(n,t)]

  let rec apply st t = match t with
    | Tv v -> begin match List.assoc_opt v st with
        | Some t2 -> t2
        | None -> Tv v
      end
    | Ta (l,r) -> Ta (apply st l, apply st r)
    | o -> o
  let rec tvs t = match t with
    | Tv v -> [v]
    | Ta (l,r) -> union (tvs l) (tvs r)
    | _ -> []

  let compose_s a b =
    let b2 = List.map (fun (v,t) -> v, apply a t) b in
    b2 @ a

  let merge_s a b = 
    let match_at var = apply a (Tv var) = apply b (Tv var) in
    let agree = List.for_all match_at @@ intersect (List.map fst a) (List.map fst b) in
    if agree 
    then Ok (compose_s a b)
    else Error "Substiutions don't agree at all points"

  let var_bind u t =
    if (kind_of t) <> u.tvar_kind 
    then Error "Kinds don't match in type var bind"
    else match t with
      | Tv v -> 
        if v = u 
        then Ok null_s 
        else Ok (single_s u t)
      | _ -> 
        if List.mem u (tvs t) 
        then Error "Recursive type var bind"
        else Ok (single_s u t)

  let rec most_general_unifier a b =
    let open Util.RM in
    match (a,b) with
    | Tc _, Tc _ -> if a = b then Ok null_s else Error "Type constructors don't match"
    | Tv av, t -> var_bind av t
    | t, Tv bv -> var_bind bv t
    | Ta (al,ar), Ta (bl,br) ->
      let* s1 = most_general_unifier al bl in
      let* s2 = most_general_unifier ar br in
      return @@ compose_s s2 s1
    | _,_ -> Error "Types don't unify"

  let rec match_s a b =
    let open Util.RM in
    match (a,b) with
    | Ta (al,ar), Ta (bl,br) ->
      let* sl = match_s al bl in
      let* sr = match_s ar br in
      merge_s sl sr
    | Tv u, t -> if u.tvar_kind = kind_of t then Ok (single_s u t) else Error "Kinds don't match"
    | Tc _, Tc _ -> if a = b then Ok null_s else Error "Type constructors don't match"
    | _,_ -> Error "Types don't unify"
end
open Basic


(** Module for classes and instance.
 *
 * TODO contents
 *)
module Classes = struct
  (* TODO this is where we expand constraints to be modules. ie some sig
     that looks like [type] -> mod, where type can be these qualified
     things, recursively depending back on modules *)
  type 'a qual = 
    { q_constaints : pred list
    ; q_head : 'a
    (* ^ not just ty so we can restrict to tv in the class_sig def *)
    }
  and pred = (* constraint, or predicate *)
    { p_name : name
    ; p_over : ty qual list 
    }

  type qty = ty qual

  (* TODO should superclasses be a feature of the class or of the instance?
   *
   * It makes sense to include them here, but it's not clear how
   * to generalize to non-single head constraints
   * *)
  type class_sig =
    { cs_args : tvar qual
    ; cs_contents : (name * ty) list
    }

  type instance_body =
    { inst_of : class_sig
    (* orders match signiture, zip to pair them *)
    ; inst_args : qty list
    ; inst_contents : int list (* TODO should match expr/binding elsewhere *)
    }

  type class_t =
    { class_instances : pred list
    (* TODO add sig here and force add_class to specify *)
    }

  type class_env =
    { classes : (name * class_t) list
    ; defaults : ty list
    }

  type ce_trans = class_env -> class_env option

  let rec apply_q s qt =
    let new_constraints = List.map (apply_c s) qt.q_constaints in
    let new_head = apply s qt.q_head in
    { q_constaints = new_constraints
    ; q_head = new_head
    }    
  and apply_c s c =
    { p_name = c.p_name
    ; p_over = List.map (apply_q s) c.p_over 
    }
    
  let rec tvs_q qt =
    let in_head = tvs qt.q_head in
    let in_quals = List.concat_map tvs_c qt.q_constaints in
    in_quals @ in_head
  and tvs_c c = List.concat_map tvs_q c.p_over

  (* TODO
   *
   * mgu and match over predicates will fail if order
   * differs in constraints, or if predicates don't match,
   * even when testing a with b and a entails b. 
   *)
  
  let rec most_general_unifier_q qa qb =
    let open Util.RM in
    let ms = List.map2 most_general_unifier_c qa.q_constaints qb.q_constaints in
    List.fold_left (fun a b -> liftM2 merge_s a b |> join) (return null_s) ms
  and most_general_unifier_c pa pb =
    let open Util.RM in
    if Name.compare pa.p_name pb.p_name <> 0 then Error "Classes Differ"
    else
      let ms = List.map2 most_general_unifier_q pa.p_over pb.p_over in
      List.fold_left (fun a b -> liftM2 merge_s a b |> join) (return null_s) ms

  let rec match_q qa qb =
    let open Util.RM in
    let ms = List.map2 match_c qa.q_constaints qb.q_constaints in
    List.fold_left (fun a b -> liftM2 merge_s a b |> join) (return null_s) ms
  and match_c pa pb =
    let open Util.RM in
    if Name.compare pa.p_name pb.p_name <> 0 then Error "Classes Differ"
    else
      let ms = List.map2 match_q pa.p_over pb.p_over in
      List.fold_left (fun a b -> liftM2 merge_s a b |> join) (return null_s) ms

  let modify ce n cl =
    { defaults = ce.defaults
    ; classes = (n,cl)::ce.classes
    }

  let initial_ce = { defaults = [] ; classes = [] } (* TODO *)

  let defined_ce n ce =
    List.fold_left (fun acc c -> acc || (fst c = n)) false ce.classes

  let compose_cet (f: ce_trans) (g: ce_trans) ce = Option.bind (f ce) g

  let add_class i is ce =
    if defined_ce i ce then raise @@ Failure "class can't be defined twice"
    else if not @@ List.for_all (fun x -> List.mem x ce.classes) is
    then raise @@ Failure "Missing Prereq class"
    else Option.some @@ modify ce i {class_instances = []}

  (* TODO here too we want a stronger check, since this will report
   * false negatives for different orders etc *)
  let inst_overlap p q = Result.is_ok @@ most_general_unifier_c p q

  let add_instance ps pred ce =
    let i = pred.p_name in
    let overlaps = List.fold_left (fun acc is -> acc || (inst_overlap pred is)) false ps in
    if not @@ defined_ce i ce then raise @@ Failure "Instance for undefined class"
    else if overlaps then raise @@ Failure "No overlapping instances TODO"
    else
      let its = List.assoc_opt i ce.classes |> Option.value ~default:{class_instances=[]} in
      let c = { p_name = i
              ; p_over = pred.p_over
              }
      in
      modify ce i { class_instances = c::(its.class_instances)}
end
open Classes

let entail_by_inst ce p =
  let open Util.RM in
  let c = List.assoc p.p_name ce.classes in
  let its = c.class_instances in
  let helper inst =
    let* subst = match_c inst p in
    return @@ List.concat_map (fun qt -> List.map (apply_c subst) qt.q_constaints) inst.p_over
  in
  List.fold_left (fun acc inst -> acc <|> helper inst) (Error "No matching instances") its

let rec entail ce ps p =
  if List.mem p ps then true
  else 
    match entail_by_inst ce p with
    | Error _ -> false
    | Ok qs -> List.for_all (entail ce ps) qs
