(* parse a charstream into the cst *)

open In_channel

open Option
open Result
open Buffer

open Cst

module Lex : sig
  type token =
    | Number of int
    | Symbol of string
    | OParen
    | CParen
    | Colon
    | Dot
    | Lambda
    | Question
    | Equal

  val channel : In_channel.t ref
  val get_token : unit -> token option
end = struct 
  let peeked = ref none
  let channel = ref In_channel.stdin

  type token =
    | Number of int
    | Symbol of string
    | OParen
    | CParen
    | Colon
    | Dot
    | Lambda
    | Question
    | Equal

  let getc () =
    match !peeked with
    | None -> input_char !channel
    | Some c -> peeked:=none; Some c
  
  let push_back c =
    match !peeked with
    | None -> peeked := Some c
    | Some _ -> raise @@ Failure "Pushed back too many chars"

  let rec read_while buf pred =
    match getc () with
    | None -> true
    | Some c ->
      if pred c
      then (Buffer.add_char buf c; read_while buf pred)
      else (push_back c; false)


  let is_whitespace c = c == ' ' || c == '\t' || c == '\n'
  let is_digit c = Char.compare c '9' <= 0 && Char.compare '0' c <= 0
  let value_of c = Char.code c - Char.code '0'

  let rec get_token () =
    let read_number_helper c =
      let buf = Buffer.create 4 in
      Buffer.add_char buf c;
      let _ = read_while buf is_digit in
      let str = Buffer.contents buf in
      let accumulate acc x = acc*10 + (value_of x) in
      let value = String.fold_left accumulate 0 str in
      value
    in
    match getc () with
    | None -> None
    | Some ' ' | Some '\n' | Some '\t' -> get_token ()
    | Some ('0'..'9' as c) ->
      let value = read_number_helper c in
      some @@ Number value
    | Some '-' -> (* TODO should this be built in the grammar? *)
      (match getc () with
       | Some ('0'..'9' as c) ->
         let value = read_number_helper c in
         some @@ Number (-1 * value)
       | _ -> raise (Failure "Minus followed by non-number"))
    | Some '(' -> some OParen
    | Some ')' -> some CParen
    | Some ':' -> some Colon
    | Some '.' -> some Dot
    | Some '\\' -> some Lambda
    | Some '?' -> some Question
    | Some '=' -> some Equal
    | Some c ->
      let buf = Buffer.create 8 in
      Buffer.add_char buf c;
      let _ = read_while buf (fun x -> not @@ is_whitespace x) in
      let str = Buffer.contents buf in
      some @@ Symbol str
end

(* Intended grammar (subject to major changes)
 * {} means optional
 * "" means literal
 * () measn grouping
 * * means 0 or more
 * + means 1 or more
 * | means or
 * UString is a sequence of 1 or more characters, starting with a
 *   captial letter.
 * LString is any non-empty string that doesn't start with a capital letter
 * Number is 1 or more digits with an optional "-" before it
 * 
 * Type := "Int" | "(" Type Type {Type*} ")"
 * Binding :=  LString ":" Type
 * Application := "(" Expression+ ")"
 * VarRef := LString
 * Function := "\ " Binding+ "." Expression
 * Assignment := Binding "=" Expression
 * LetBlock := "?" "(" Assignment+ ")" "." Expression
 * Expression := Number | VarRef | Function | Application | LetBlock
 *)
module Parse = struct
  module Parser : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
    val (<|>) : 'a t -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val seq : 'a t -> 'b t -> 'b t
    val (>>) : 'a t -> 'b t -> 'b t
    val run_empty : 'a t -> ('a, unit) result
    val next : Lex.token option t
    val fail : 'a t
  end = struct 
    type ctx = {
      consumed: Lex.token list;
      peeked: Lex.token list;
    }
    type 'a t = {
      run: ctx -> (('a * ctx), ctx) result
    }

    let fail =
      let run = fun ctx -> error ctx in
      {run=run}
    let next =
      let run = fun ctx ->
      match ctx.peeked with
      | x::xs ->
        let ctx2 = {consumed=x::ctx.consumed; peeked=xs} in
        ok (some x, ctx2)
      | [] ->
        let tko = Lex.get_token () in
        (match tko with
         | None -> ok (none, ctx)
         | Some tk ->
           let ctx2 = {consumed=tk::ctx.consumed; peeked=[]} in
           ok (some tk, ctx2))
      in {run=run}
    let return a =
      let run = fun ctx -> ok (a, ctx)
      in {run=run}
    let bind a f =
      let run = fun ctx -> 
      match a.run ctx with
      | Error c -> Error c
      | Ok (x, ctx2) -> (f x).run ctx2
      in {run=run}
    let (>>=) = bind
    let (let*) = bind
    let (<|>) a b =
      let run = fun ctx ->
      match a.run ctx with
      | Ok (x, ctx2) -> Ok (x, ctx2)
      | Error ctx2 ->
        let ctx3 = {
          consumed=ctx.consumed;
          peeked=(List.rev_append ctx2.consumed ctx.peeked)
        } in
        b.run ctx3
      in {run=run}
    let map f a = a >>= fun v -> return @@ f v
    let (>>|) a f = map f a
    let seq a b = a >>= fun _ -> b
    let (>>) = seq
    let run_empty c =
      match c.run {consumed=[];peeked=[]} with
      | Error _ -> error ()
      | Ok (a,_) -> ok a
  end
  open Parser

  let literal l =
    let* n = next in
    match n with
    | Some s -> if s == l then return l else fail
    | _ -> fail

  let symbol =
    let* n = next in
    match n with
    | Some (Symbol s) -> return s
    | _ -> fail

  let number =
    let* n = next in
    match n with
    | Some (Number i) -> return i
    | _ -> fail
  
  let ustring =
    let* s = symbol in
    match Seq.uncons (String.to_seq s) with
    | Some (c, _) ->
      if Char.code c >= Char.code 'A' &&
         Char.code c <= Char.code 'Z'
      then return s
      else fail      
    | None -> fail

  let lstring =
    let* s = symbol in
    match Seq.uncons (String.to_seq s) with
    | Some (c, _) ->
      if Char.code c < Char.code 'A' ||
         Char.code c > Char.code 'Z'
      then return s
      else fail      
    | None -> fail

  let rec parse_many p =
    let* x = p in
    let* tail = parse_many p in
    return (x :: tail) <|> (return [])

  let parse_many1 p =
    let* inner = parse_many p in
    match inner with
    | _x::_xs -> return inner
    | [] -> fail    
  
  let parse_type =
    let rec parse_type1 () =
      (literal (Symbol "Int") >> return Int_t) <|>
      (literal OParen >>
       let* types = (parse_many (parse_type1 ())) >>| List.rev in
       let* out = (match types with 
           | final::(_penult::_rest as args) ->
             return @@ Fun_t ((Array.of_list @@ List.rev args), final)
           | _ -> fail)
       in
       literal CParen >>
       return out
      )
    in parse_type1 ()

  let parse_name = lstring
    
  let parse_binding =
    let* l = parse_name in
    literal Colon >>
    let* tp = parse_type in
    return (l, tp)
  
  let parse_expression =
    let rec parse_expr1 () =
      (number >>| fun i -> Lit i) <|>
      (parse_name >>| fun n -> Var n) <|>
      (literal OParen >>
       let* inner = parse_many1 (parse_expr1 ()) in
       return (App (List.hd inner, List.tl inner))) <|>
      (literal Lambda >>
       let* binds = parse_many1 parse_binding in
       literal Dot >>
       let* body = parse_expr1 () in
       return @@ Fun (binds, body)) <|>
      (literal Question >>
       literal OParen >>
       let* assigns = parse_many1 (parse_assignment ()) in
       literal CParen >>
       literal Dot >>
       let* body = parse_expr1 () in
       return @@ Let (assigns, body)) 
    and parse_assignment () =
      let* bind = parse_binding in
      literal Equal >>
      let* expr = parse_expr1 () in
      return (bind,expr)
    in
    parse_expr1 ()
end
