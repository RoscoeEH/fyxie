open Name

(* https://web.cecs.pdx.edu/~mpj/thih/thih.pdf *)

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


(* TODO this is where we expand constraints to be modules. ie some sig
   that looks like [type] -> mod, where type can be these qualified
   things, recursively depending back on modules *)
type 'a qual = (* a 'a, qualified by a list of constraints *)
  { q_constaints : cntr list
  ; q_head : 'a 
  }
and cntr = (* constraint, or predicate *)
  { cntr_name : name
  ; cntr_t : ty list 
  }

let apply_c s c =
  { cntr_name = c.cntr_name
  ; cntr_t = c.cntr_t |> List.map @@ apply s 
  }
let tvs_c c = List.concat_map tvs c.cntr_t

let apply_q s q =
  { q_constaints = q.q_constaints |> List.map (apply_c s)
  ; q_head = q.q_head |> apply s 
  }
let tvs_q q = List.map tvs_c q.q_constaints @ [tvs q.q_head]

let m_fold
    (m : ty -> ty -> (subst, 'b) result)
    (f : subst -> subst -> (subst, 'b) result)
    a b =
  let open Util.RM in
  let ms = List.map2 m a.cntr_t b.cntr_t in
  let fold_help acc ra = f <$> acc <*> ra in
  List.fold_left fold_help (return null_s) ms
;;

let lift_c m f a b = (* TODO not sure about this *)
  if Name.compare b.cntr_name a.cntr_name = 0
  then m_fold m f a b
  else Error "Classes differ"
;;

let most_general_unifier_c = lift_c most_general_unifier merge_s
  
let match_c = lift_c match_s merge_s

(* TODO should superclasses be a feature of the class or of the instance? *)
type class_sig =
  { cs_args : tvar qual
  ; cs_contents : (name * ty) list
  }

type instance_body =
  { inst_of : class_sig
  (* orders match signiture, zip to pair them *)
  ; inst_args : ty qual list
  ; inst_contents : int list (* TODO should match expr/binding elsewhere *)
  }

type class_t =
  { class_instances : ty qual list list
  }

type class_env =
  { classes : (name * class_t) list
  ; defaults : ty list
  }

let modify ce n cl =
  { defaults = ce.defaults
  ; classes = (n,cl)::ce.classes
  }
  
(* TODO *)
let initial_ce = { defaults = [] ; classes = [] }

type ce_trans = class_env -> class_env option

let compose_cet (f: ce_trans) (g: ce_trans) ce = Option.bind (f ce) g
