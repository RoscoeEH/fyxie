
open Result

(* TODO this will need a rework imminently *)

let separator = '_'
(* NOTE chosen not to overlap w/ parsed characters, should change to . later *)

(* name segment w/o separators *)
type name_atom = string         

(* sequence of name segments with at least a terminal segment *)
type name =
  { mods : name_atom list       (* must start with cap letter *)
  ; term : name_atom
  }

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

let name_of_string s =
  let check_upper p =
    if String.length p == 0 then Error "Empty name segment"
    else match String.get p 0 with
      | 'A'..'Z' -> Ok p
      | 'a'..'z' -> error @@ "Name " ^ p ^ " not uppercase"
      | _ -> error @@ "Unexpected char in name " ^ p
  in
  let parts = String.split_on_char separator s in
  match List.rev parts with
  | [] -> Error "Empty name"
  | term::rest ->
    match Util.sequence @@ List.rev_map check_upper rest with
    | Error e -> Error e
    | Ok ps -> Ok {mods=ps; term=term}
;;  

let compare a b =
  let helper ap bp =
    List.map (fun (a,b) -> String.compare a b) @@ List.combine ap bp  
  in
  match List.find_opt (fun a -> a <> 0) (helper a.mods b.mods) with
  | Some i -> i
  | None -> String.compare a.term b.term
;;
