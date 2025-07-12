(* parse a charstream into the cst *)

open In_channel

open Option
open Either
open Result
open Buffer

open Cst

module Lex : sig
  type token =
    | Number of int
    | Symbol of string
    | OParen
    | CParen

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
    | Some c ->
      let buf = Buffer.create 8 in
      Buffer.add_char buf c;
      let _ = read_while buf (fun x -> not @@ is_whitespace x) in
      let str = Buffer.contents buf in
      some @@ Symbol str
end
