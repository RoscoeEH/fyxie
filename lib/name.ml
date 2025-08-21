
open Util.RM

(* TODO this will need a rework imminently *)

let separator = '_'
(* NOTE chosen not to overlap w/ parsed characters, should change to . later *)

(* name segment w/o separators *)
type name_atom = string         

(* sequence of name segments with at least a terminal segment *)
type name =
  { mods : name_atom list       (* must start with cap letter, in root to leaf order *)
  ; term : name_atom
  }

let pp_name_atom n = n

let pp_name n =
  let da = Buffer.create 8 in
  let helper p =
    Buffer.add_string da p;
    Buffer.add_char da separator
  in
  List.iter helper n.mods;
  Buffer.add_string da n.term;
  Buffer.contents da
;;

let check_upper p =
  if String.length p == 0 then Error "Empty name segment"
  else match String.get p 0 with
    | 'A'..'Z' -> Ok p
    | 'a'..'z' -> error @@ "Name " ^ p ^ " not uppercase"
    | _ -> error @@ "Unexpected char in name " ^ p
;;

let name_atom_of_string s =
  let parts = String.split_on_char separator s in
  match parts with
  | [] -> Error "Empty name"
  | [x] -> Ok x
  | _ -> Error "More than one segment in name atom"
;;

let name_of_string s =
  let parts = String.split_on_char separator s in
  match List.rev parts with
  | [] -> Error "Empty name"
  | term::rest ->
    match Util.sequence @@ List.rev_map check_upper rest with
    | Error e -> Error e
    | Ok ps -> Ok {mods=ps; term=term}
;;  

let compare a b =
  match List.find_opt (fun a -> a <> 0) (List.map2 String.compare a.mods b.mods) with
  | Some i -> i
  | None -> String.compare a.term b.term
;;

let add_prefix p n =
  let* p = check_upper p in
  return { mods = p::n.mods ; term = n.term }
;;

let drop_prefix n =
  match List.rev n.mods with
  | _::rest -> return {mods=rest ; term=n.term}
  | [] -> error "Name has no prefix"
;;
