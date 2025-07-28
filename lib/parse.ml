(* convert input stream to cst, define syntax *)

open In_channel
open Option
open Result
open Ast

(* Converts strings to concrete tokens *)
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
  val pp_token : token -> string
end = struct
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

  let pp_token t =
    match t with
    | Number i -> "{Number " ^ string_of_int i ^ "}"
    | Symbol s -> "{Symbol " ^ s ^ "}"
    | OParen -> "("
    | CParen -> ")"
    | Colon -> ":"
    | Dot -> "."
    | Lambda -> "λ"
    | Question -> "?"
    | Equal -> "="
  ;;

  let peeked = ref none
  let channel = ref In_channel.stdin

  let getc () =
    match !peeked with
    | None -> input_char !channel
    | Some c ->
      peeked := none;
      Some c
  ;;

  let push_back c =
    match !peeked with
    | None -> peeked := Some c
    | Some _ -> raise @@ Failure "Pushed back too many chars"
  ;;

  let rec read_while buf pred =
    match getc () with
    | None -> true
    | Some c ->
      if pred c
      then (
        Buffer.add_char buf c;
        read_while buf pred)
      else (
        push_back c;
        false)
  ;;

  let is_whitespace c = c == ' ' || c == '\t' || c == '\n'
  let is_digit c = Char.compare c '9' <= 0 && Char.compare '0' c <= 0

  let is_reserved c =
    match c with
    | '(' | ')' | '\\' | '?' | '.' | '=' | ':' -> true
    | _ -> false
  ;;

  let value_of c = Char.code c - Char.code '0'

  let rec get_token () =
    let read_number_helper c =
      let buf = Buffer.create 4 in
      Buffer.add_char buf c;
      let _ = read_while buf is_digit in
      let str = Buffer.contents buf in
      let accumulate acc x = (acc * 10) + value_of x in
      let value = String.fold_left accumulate 0 str in
      value
    in
    match getc () with
    | None -> None
    | Some ' ' | Some '\n' | Some '\t' -> get_token ()
    | Some ('0' .. '9' as c) ->
      let value = read_number_helper c in
      some @@ Number value
    | Some '-' ->
      (* TODO should this be built in the grammar? *)
      (match getc () with
       | Some ('0' .. '9' as c) ->
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
      let _ = read_while buf (fun x -> not (is_whitespace x || is_reserved x)) in
      let str = Buffer.contents buf in
      some @@ Symbol str
  ;;
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
    type ctx
    type 'a t = ctx -> ('a * ctx, string * ctx) result

    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val seq : 'a t -> 'b t -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
    val empty_context : ctx
    val next : Lex.token option t
    val fail : string -> 'a t
    val fix_m : ('a t -> 'a t) -> 'a t
  end = struct
    type ctx =
      { consumed : Lex.token list
      ; peeked : Lex.token list
      }

    type 'a t = ctx -> ('a * ctx, string * ctx) result

    (* specific monadic version, in signature *)
    let fix_m f = Util.y f 
    
    let fail msg ctx = error (msg,ctx)

    let next ctx =
      match ctx.peeked with
      | x :: xs ->
        let ctx2 = { consumed = x :: ctx.consumed; peeked = xs } in
        ok (some x, ctx2)
      | [] ->
        let tko = Lex.get_token () in
        (match tko with
         | None -> ok (none, ctx)
         | Some tk ->
           let ctx2 = { consumed = tk :: ctx.consumed; peeked = [] } in
           ok (some tk, ctx2))
    ;;

    let return a ctx = ok (a, ctx)

    let bind a f ctx =
      match a ctx with
      | Error c -> Error c
      | Ok (x, ctx2) -> (f x) ctx2
    ;;

    let ( >>= ) = bind
    let ( let* ) = bind

    let ( <|> ) a b ctx =
      (* idea is to thread peeked through both, but have consumed be reset for the second alterative *)
      let ctx' = {consumed=[]; peeked=ctx.peeked} in
        match a ctx' with
        | Ok (x, ctx2) ->
          let ctx2' = {consumed=ctx2.consumed @ ctx.consumed; peeked=ctx2.peeked} in
          Ok (x, ctx2')
        | Error (msg1, ctx2) ->
          let ctx3 =
            { consumed = ctx.consumed; peeked = List.rev_append ctx2.consumed ctx.peeked }
          in
          b ctx3 |> Result.map_error @@ fun (msg2, c) -> "Alternative failed:\n  " ^ msg1 ^ "\nand\n  " ^ msg2 , c
    ;;

    let map f a = a >>= fun v -> return @@ f v
    let ( >>| ) a f = map f a
    let seq a b = a >>= fun _ -> b
    let ( >> ) = seq
    let empty_context = { consumed = []; peeked = [] }
  end

  open Parser

  let literal l =
    let* n = next in
    match n with
    | Some s ->
      if s = l
      then return l
      else fail @@ "Expected " ^ Lex.pp_token l ^ " got " ^ Lex.pp_token s
    | _ -> fail "Unexpected EOF"
  ;;

  let symbol =
    let* n = next in
    match n with
    | Some (Symbol s) -> return s
    | None -> fail "Unexpected EOF"
    | Some t -> fail @@ "Expected symbol got " ^ Lex.pp_token t
  ;;

  let number =
    let* n = next in
    match n with
    | Some (Number i) -> return i
    | None -> fail "Unexpected EOF"
    | Some t -> fail @@ "Expected number got " ^ Lex.pp_token t
  ;;

  let ustring =
    let* s = symbol in
    match Seq.uncons (String.to_seq s) with
    | Some (c, _) ->
      if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z'
      then return s
      else fail @@ "Symbol " ^ s ^ " not uppercase"
    | None -> fail @@ "Unexpected empty string. This shouldn't happen"
  ;;

  let lstring =
    let* s = symbol in
    match Seq.uncons (String.to_seq s) with
    | Some (c, _) ->
      if Char.code c < Char.code 'A' || Char.code c > Char.code 'Z'
      then return s
      else fail @@ "Symbol " ^ s ^ " not uppercase"
    | None -> fail "Unexpected empty string. This shouldn't happen"
  ;;

  let rec many p =
    (let* x = p in
     let* tail = many p in
     return (x :: tail))
    <|> return []
  ;;

  let many1 p =
    let* inner = many p in
    match inner with
    | _x :: _xs -> return inner
    | [] -> fail "Zero repetitions but one was required."
    (* TODO more info, but how? *)
  ;;

  let in_parens p =
    let* _ = literal OParen in
    let* v = p in
    let* _ = literal CParen in
    return v
  ;;

  let parse_type =
    let parse_type1 (self : type_t t) =
      let base = literal (Symbol "Int") >> return Int_t in
      let func = in_parens 
        (let* types = many self >>| List.rev in
          match types with
          | final :: (_penult :: _rest as args) ->
            return @@ Fun_t (Array.of_list @@ List.rev args, final)
          | _ -> fail "Function type with only one element")
      in
      base <|> func
    in
    fix_m parse_type1
  ;;

  let parse_name = lstring

  let parse_binding =
    let* l = parse_name in
    literal Colon
    >>
    let* tp = parse_type in
    return (l, tp)
  ;;

  let parse_expression =
    let parse_expr1 (self : expr t) =
      let num = number >>| fun i -> Lit i in
      let name = parse_name >>| fun n -> Var n in
      let app =
        in_parens
          (let* inner = many1 self in
           return (App (List.hd inner, List.tl inner)))
      in
      let lam =
        literal Lambda
        >>
        let* binds = many1 parse_binding in
        literal Dot
        >>
        let* body = self in
        return @@ Fun (binds, body)
      in
      let l_block =
        literal Question
        >>
        let* assigns =
          in_parens
            (let parse_assignment =
               let* bind = parse_binding in
               literal Equal
               >>
               let* expr = self in
               return (bind, expr)
             in
             many1 @@ parse_assignment)
        in
        literal Dot
        >>
        let* body = self in
        return @@ Let (assigns, body)
      in
      num <|> name <|> app <|> lam <|> l_block
    in
    fix_m parse_expr1
  ;;
end
