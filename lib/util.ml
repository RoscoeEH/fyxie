(* Some small modules that show up in other places. Many of them could likely be replaced *)

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
module RM = struct
  open Result

  type e
  type 'a t = ('a, e) result

  let bind a f =
    match a with
    | Error e -> Error e
    | Ok v -> f v
  ;;

  let ( >>= ) = bind
  let return a = Ok a

  let map f a =
    match a with
    | Error e -> Error e
    | Ok v -> Ok (f v)
  ;;

  let ( >>| ) a f = map f a

  let ( <|> ) a b =
    match a with
    | Ok a -> Ok a
    | Error _ -> b
  ;;

  let ( >> ) a b =
    match a with
    | Error e -> Error e
    | Ok _ -> b
  ;;

  let ( let* ) a f = bind a f

  let map_err f a =
    match a with
    | Ok v -> Ok v
    | Error e -> Error (f e)
  ;;
end

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

let sequence_arr (arr : ('a, 'e) result array) =
  let open Result in
  let helper acc a =
    match acc with
    | Error e -> error e
    | Ok () -> (match a with
        | Error e -> error e
        | Ok _ -> ok ())
  in
  match Array.fold_left helper (ok ()) arr with
  | Error e -> error e
  | Ok () -> ok @@ Array.map Result.get_ok arr
;;

(* Pretty printing *)
module Pretty : sig
  val inc_indent : unit -> unit
  val dec_indent : unit -> unit
  val indent_line : string -> string
  val pp_lst : ?sep:string -> ('a -> string) -> 'a list  -> string
  val pp_arr : ?sep:string -> ('a -> string) -> 'a array -> string
end = struct
  let indent_lvl = ref 0
  let indent_str () = String.make (!indent_lvl * 2) ' '
  let inc_indent () = indent_lvl := !indent_lvl + 1
  let dec_indent () = indent_lvl := !indent_lvl - 1
  let indent_line str = indent_str () ^ str
  let pp_lst ?(sep=" ") pp lst = List.fold_left  (fun acc x -> acc ^ indent_line (pp x) ^ sep) "" lst
  let pp_arr ?(sep=" ") pp arr = Array.fold_left (fun acc x -> acc ^ indent_line (pp x) ^ sep) "" arr
end
