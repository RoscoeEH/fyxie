(*
 * This module contains a bytecode that one could compile the AST into,
 * and a garbage collector that understands said bytecode's treatment
 * of pointers, opcodes, and integers.
 *
 * TODO the code here (particularly the garbage collection code) is a
 * really strange mix of imperative and functional. I think that's
 * fine, but it could probably do with some types to make it a bit
 * more readable, since currently the relationship between ocaml int,
 * slot, and versions of those representing numbers, pointers,
 * indexes, etc are all sort of muddled together.
 *)
open Option

(* couldn't find an existing monad signature for option *)
module OM = struct
  type 'a t = 'a option
  let bind a f = match a with
    | None -> None
    | Some v -> f v

  let (>>=) = bind
  let return a = Some a

  let map f a = match a with
    | None -> None
    | Some v -> Some (f v)
  let (>>|) a f = map f a

  let (<|>) a b = match a with
    | Some v -> Some v
    | None -> b

  let (>>) a b = match a with
    | None -> None
    | Some _ -> b

  let (let*) a f = bind a f
end
open OM

(*
 * module Interpreted : BC = struct
 *)
type bc_op =
  | PushLit of int
  | ResStack of int
  | FetchSp of int
  | SetSp of int
  | Dup
  | Alloc of int
  | Fetch of int
  | Set of int
  | Call
  | Ret
type slot =
  | Num of int
  | Op of bc_op
type 'a ctx = 'a


let is_pointer s = match s with
  | Op _ -> false
  | Num i -> Int.logand i 1 == 1

let as_pointer s = match s with
  | Op _ -> none
  | Num i -> if Int.logand i 1 == 1
    then some(Int.shift_right i 1)
    else none
let zero = Num 0
let as_int s = match s with
  | Op _ -> none
  | Num i -> if Int.logand i 1 == 1
    then none
    else some(Int.shift_right i 1)
let from_int_as_num i = Num (Int.shift_left i 1)
let from_int_as_ptr i = Num (Int.logor (Int.shift_left i 1) 1)

let to_slot op = Op op
let to_opcode slot = match slot with
  | Num _ -> none
  | Op o -> some o

let push_lit n = PushLit n
let reserve_stack n = ResStack n
let fetch_stack n = FetchSp n
let set_stack_x n = SetSp n
let dup_x = fetch_stack 0
let alloc n = Alloc n
let fetch_x n = Fetch n
let set_x_y n = Set n
let call_x = Call
let return_x = Ret
(*
 * end
 *)


(* internal details of interpreted memory and garbage collection *)
let mem_size = 4096           (* in slots *)
let heap_start = 2048
let heap_end = 4097
let heap_mid = (heap_end - heap_start) / 2
let stack_start = heap_start - 1
let mem = Array.make mem_size zero
let sp = ref 0
let hp = ref heap_start

(* infix inclusive range list syntax *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n ::acc)
  in aux j []

let fwd_addr ptr = as_pointer (mem.(ptr))
let copy_and_fwd ptr nh_top =
  as_int mem.(ptr) >>| fun len ->
    let _ = Array.blit mem ptr mem !nh_top len in
    let _ = Array.set mem ptr (from_int_as_ptr ptr) in
    let hold = !nh_top in
    let _ = nh_top := !nh_top + len in
    hold

let handle_heap_ptr ptr nh_top =
  fwd_addr ptr <|>
  copy_and_fwd ptr nh_top

let garbage_collect () =
  let old_heap = if !hp > heap_mid then heap_mid else heap_start in
  let old_end = old_heap + (heap_mid - heap_start) in
  let new_heap = if old_heap == heap_mid then heap_start else heap_mid in
  let new_heap_next = ref new_heap in
  let on_old_heap p = p >= old_heap && p < old_end in
  let maybe_copy_slot i =
    value (
      let* p = as_pointer mem.(i) in                (* is a ptr *)
      let* p = if on_old_heap p                     (* is on old heap *)
        then some p
        else none in
      let* np = handle_heap_ptr p new_heap_next in  (* copy or follow fwd *)
      return (Array.set mem i (from_int_as_ptr np)) (* update the old ptr *)
    ) ~default:()                                   (* collapse unit option to unit *)
  in
  for i = !sp to stack_start do
    maybe_copy_slot i
  done ;                        (* copy initial heap contents from stack *)
  let i = ref new_heap in
  while !i < !new_heap_next do
    maybe_copy_slot !i ;
    i := !i + 1
  done ;                        (* traverse new heap, copying pointers from old to new causing growth *)
  hp := !new_heap_next          (* finalize, swap to new heap *)

let bump_maybe_gc n =
  if !hp < heap_mid && !hp + n + 1 >= heap_mid then
    let _ = garbage_collect () in
    if !hp + n + 1>= heap_end then raise (Failure "OOM")
    else
      let h = !hp + 1 in
      let _ = hp := h + n in h
  else if !hp + n + 1 >= heap_end then
    let _ = garbage_collect () in
    if !hp + n + 1 >= heap_mid then raise (Failure "OOM")
    else
      let h = !hp + 1 in
      let _ = hp := h + n in h
  else
    let h = !hp + 1 in
    let _ = hp := h + n in h

(* TODO define runner based on bytecode interpretor + garbage collection *)
(* TODO do I want to compile into an array and then run that? or
 * actually walk the tree and do things live? I'm sort of leaning
 * towards compiled since I think that will be easier and also what we
 * want in the longer term. *)
