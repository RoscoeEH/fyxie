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

(** Module for making strings
*)
module PrettyPrint = struct
  let rec pp_kind k =
    match k with
    | Star -> "*"
    | Kf (a,b) -> pp_kind a ^ " -> " ^ pp_kind b
  ;;

  let rec pp_type ?(_mods=[]) t =
    match t with
    | Tc (n,k) ->
      pp_name n ^ "{kind " ^ pp_kind k ^ "}"
    | Ta (a,b) ->
      "{app " ^ pp_type a ^ " to " ^pp_type b ^"}"
    | _ -> raise @@ Failure "More complicated types"
  ;;
end
open PrettyPrint

(** Module for builtins
 *)
module Builtins = struct
  let unit_t : ty = Tc (Result.get_ok @@ Name.name_of_string "Unit", Star)
  let int_t : ty = Tc (Result.get_ok @@ Name.name_of_string "Int", Star)
  let arrow_t : ty = Tc (Result.get_ok @@ Name.name_of_string "->", Kf (Star, (Kf (Star, Star))))

  open Util.RM
  
  let as_function t =
    match t with
    | Ta (Ta (arrow, a), b) ->
      if arrow == arrow_t
      then return (a,b)
      else Error (pp_type t ^ " not a function type")
    | _ -> Error (pp_type t ^ " not a function type")

  let mk_function a b = Ta (b, (Ta (arrow_t, a)))
  
  let rec eq_exact a b =
    match (a,b) with
    | Tc _, Tc _ -> a == b
    | Tg i, Tg j -> i == j
    | Tv v, Tv w -> v == w
    | Ta (a1,a2), Ta (b1,b2) -> eq_exact a1 b1 && eq_exact a2 b2
    | _,_ -> false

  let unify a b = match_s a b >>| fun s -> apply s a
end
open Builtins
    
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

  let rec is_acyclic q =
    (* since q is strictly in its recursion and instantiated,
     * it must either be finite acyclic or cyclic.
     * Thus physical inequality is enough to rule out cycles. *)
    match q.q_constaints with
    | [] -> true
    | cs ->
      let help acc q' =
        acc && q' != q && is_acyclic q'
      in
      let help_p acc c =
        List.fold_left help acc c.p_over
      in
      List.fold_left help_p true cs
  ;;
  
  let make_q h cs =
    let q = {q_constaints=cs;q_head=h} in
    if is_acyclic q
    then q
    else raise @@ Failure "Qualifed type is cyclic"
  ;;
  
  type qty = ty qual

  type class_sig =
    { cs_args : tvar qual
    ; cs_contents : (name * ty) list
    }

  type instance_body =
    { inst_of : class_sig
    (* orders match signiture, zip to pair them *)
    ; inst_args : qty list
    ; class_name : name
    ; inst_contents : int list (* TODO should match expr/binding elsewhere *)
    }

  type class_t =
    { class_instances : instance_body list
    (* TODO enfoce that all instances match teh class sig, but that
       match is a structural one, eg kind and arity "skeleton" match,
       not that the types/tvars match *)
    }

  type class_env =
    { classes : (name * class_t) list
    ; defaults : ty list
    }

  type ce_trans = class_env -> class_env option

  let rec apply_q s qt =
    let new_constraints = List.map (apply_c s) qt.q_constaints in
    let new_head = apply s qt.q_head in
    make_q new_head new_constraints
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
  mgu and match over predicates will fail if order differs in
  constraints, or if predicates don't match, even when testing a with
  b and a entails b.
  
  pred and qual should use sets, but defining a total order over
  mutually recursive types is complicated. We could maybe do something
  like make them exist in some sort of arena and then use indecies to
  order them? But it would be really nice to just traverse them
  transparently like we can now.

  As always some sort of monad that gives syntax for a fetch/set into
  an arena could help a bit, but that's a lot of boilerplate that I
  don't want to deal with right now. Maybe if this was haskell :(
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
    List.exists (fun c -> (fst c = n)) ce.classes

  let compose_cet (f: ce_trans) (g: ce_trans) ce = Option.bind (f ce) g

  let add_class i is ce =
    if defined_ce i ce then raise @@ Failure "class can't be defined twice"
    else if not @@ List.for_all (fun x -> List.mem x ce.classes) is
    then raise @@ Failure "Missing Prereq class"
    else Option.some @@ modify ce i {class_instances = []}

  let pred_of_inst ib = {p_name=ib.class_name ; p_over=ib.inst_args}

  (* TODO here too we want a stronger check, since this will report
   * false negatives for different orders etc, see above *)
  let inst_overlap a b =
    let a = pred_of_inst a in
    let b = pred_of_inst b in
    Result.is_ok @@ most_general_unifier_c a b
  ;;

  let prereqs ib =
    let concat_map f l =
      (* attempt to reduce repeats *)
      List.fold_left union [] @@ List.map f l
    in
    concat_map (fun q -> q.q_constaints) ib.inst_args
  ;;
  
  let add_instance ib ce =
    let n = ib.class_name in
    match List.assoc_opt n ce.classes with
    | None -> raise @@ Failure "Instance for undefined class"
    | Some c ->
      let overlaps =
        List.exists
          (fun other_inst -> (inst_overlap ib other_inst))
          c.class_instances
      in
      if overlaps
      then raise @@ Failure
          "No overlapping instances TODO add context management"
      else
        modify ce n { class_instances = ib::(c.class_instances) }
  ;;
end
open Classes

(** Module for instance resolution
 *)
module Resolve = struct 
  let entail_by_inst ce p =
    let open Util.RM in
    let c = List.assoc p.p_name ce.classes in
    let its = c.class_instances in
    let helper (inst : instance_body) =
      let* subst = match_c (pred_of_inst inst) p in
      let u_prereqs = prereqs inst |> List.map (apply_c subst) in
      return (inst, u_prereqs)
    in
    List.fold_left
      (fun acc inst -> acc <|> helper inst) (Error "No matching instances")
      its

(*
 * let rec entail ce known target =
 *   let test_known p =
 *     TODO
 *   in
 *   let removal =
 *     List.filter_map (fun )
 *   in
 *)

(*
 * let rec entail ce ps p =
 *   if List.mem p ps then true
 *   else 
 *     match entail_by_inst ce p with
 *     | Error _ -> false
 *     | Ok qs -> List.for_all (entail ce ps) qs
 *)
end

(* returns None for boxed pointer, Some(n) for an unboxed value of n slots *)
let stack_size t =
  match as_function t with
  | Ok _ -> None
  | Error _ -> Some (1)
