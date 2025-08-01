(* Some small modules that show up in other places. Many of them could likely be replaced *)

(*
   * generic machinery for a y combinator for anonymous recursive functions.
 * This is required since the naive thing with `let rec` doesn't seem to work
 * (it doesn't delay the recusion until runtime despite the fact that you are
 * fundamentally building a continuation. Haskell brain strikes again. :< )
 *
 * https://www.cs.cornell.edu/courses/cs3110/2012sp/lectures/lec29-fixpoints/lec29.html
 * https://gist.github.com/dhil/55cf406865209ab945d8ba1484ea615c
*)
type 'a fix = Fix of ('a fix -> 'a)

let fix x = Fix x
let unfix (Fix x) = x

let y =
  fun f ->
  let g x a = f ((unfix x) x) a in
  g (fix g)
;;

(* couldn't find an existing monad signature for option *)
module OM = struct
  include Option

  let bind a f =
    match a with
    | None -> None
    | Some v -> f v
  ;;

  let ( >>= ) = bind
  let return a = Some a

  let map f a =
    match a with
    | None -> None
    | Some v -> Some (f v)
  ;;

  let ( >>| ) a f = map f a

  let ( <|> ) a b =
    match a with
    | Some v -> Some v
    | None -> b
  ;;

  let ( >> ) a b =
    match a with
    | None -> None
    | Some _ -> b
  ;;

  let ( let* ) a f = bind a f
end

(* monad sig for result *)
(*
 * module RM = struct
 *   open Result
 * 
 *   type e
 *   type 'a t = ('a, e) result
 * 
 *   let bind a f =
 *     match a with
 *     | Error e -> Error e
 *     | Ok v -> f v
 *   ;;
 * 
 *   let ( >>= ) = bind
 *   let return a = Ok a
 * 
 *   let map f a =
 *     match a with
 *     | Error e -> Error e
 *     | Ok v -> Ok (f v)
 *   ;;
 * 
 *   let ( >>| ) a f = map f a
 * 
 *   let ( <|> ) a b =
 *     match a with
 *     | Ok a -> Ok a
 *     | Error _ -> b
 *   ;;
 * 
 *   let ( >> ) a b =
 *     match a with
 *     | Error e -> Error e
 *     | Ok _ -> b
 *   ;;
 * 
 *   let ( let* ) a f = bind a f
 * 
 *   let map_err f a =
 *     match a with
 *     | Ok v -> Ok v
 *     | Error e -> Error (f e)
 *   ;;
 * 
 * end
 *)

(* Collects Ok values, stops on an Error *)
let sequence (lst : ('a, 'e) result list) =
  let cons_ok (elm : ('a, 'e) result) (acc : ('a list, 'e) result) =
    match elm, acc with
    | Ok i, Ok j -> Ok (i :: j)
    | Error e, _ -> Error e
    | _, Error e -> Error e
  in
  let (init : ('a list, 'e) result) = Ok [] in
  List.fold_right cons_ok lst init
;;

(* Pretty printing *)
module Pretty = struct
  let indent_lvl = ref 0
  let indent_str () = String.make (!indent_lvl * 2) ' '
  let inc_indent () = indent_lvl := !indent_lvl + 1
  let dec_indent () = indent_lvl := !indent_lvl - 1
  let print str = print_string (indent_str () ^ str)
  let print_endline str = print_endline (indent_str () ^ str)
end
