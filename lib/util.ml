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

  (* Collects Ok values, stops on an Error *)
  let sequence (lst : ('a, 'e) result list) =
    let cons_ok (elm : ('a, 'e) result) (acc : ('a list, 'e) result) =
      let* a = acc in
      let* e = elm in
      Ok (e :: a)
    in
    let (init : ('a list, 'e) result) = Ok [] in
    List.fold_right cons_ok lst init
  ;;
end
