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
open Util.OM

module Interpreter = struct
  include Bytecode.BC

  (* Each slot is either an opcode or a value, distinguished by ocaml
 * case analysis.
 *
 * A value can be either an integer or a pointer, distinguished by
 * the low order bit.
 *
 * In a real compilation, one would eschew the op vs val check, and
 * just to a number/pointer check (or not even that). This tagging is
 * for garbage collection. *)

  let as_int s =
    match s with
    | Op _ -> none
    | Num i -> if Int.logand i 1 == 1 then none else some (Int.shift_right i 1)
  ;;

  let to_slot op = Op op

  let to_opcode slot =
    match slot with
    | Num _ -> none
    | Op o -> some o
  ;;

  (* internal details of interpreted memory and garbage collection *)
  let mem_len      = ref (-1) (* in slots *)
  let mem          = ref @@ Array.make 0 zero
  let heap_start   = ref (-1)
  let heap_end     = ref (-1)
  let heap_mid     = ref (-1)
  let static_start = ref (-1)
  let static_end   = ref (-1)
  let stack_start  = ref (-1)
  let stack_end    = ref (-1)
  let sp           = ref (-1)
  let hp           = ref (-1)
  let pc           = ref (-1)

  let init_mem ~mem_size
      ~static_off ~statics
      ~stack_size ~stack_off
      ~heap_size ~heap_off
      ~code_off ~code =
    mem_len      := mem_size;
    mem          := Array.make mem_size zero;
    static_start := static_off;
    static_end   := static_off + Dynarray.length statics - 1;
    stack_start  := stack_off;
    stack_end    := stack_off + stack_size - 1;
    heap_start   := heap_off;
    heap_end     := heap_off + heap_size - 1;
    heap_mid     := (!heap_start + !heap_end) / 2;
    for i = 0 to Dynarray.length statics do
      Array.set !mem (static_off + i) @@ Dynarray.get statics i
    done;
    for i = 0 to Dynarray.length code do
      Array.set !mem (code_off + i) @@ Op (Dynarray.get code i)
    done;
    sp := !stack_end;
    hp := !heap_start;
    pc := code_off
  ;;
  
  let reset_memory () =
    mem := Array.make 0 zero;
    mem_len      := (-1);
    heap_start   := (-1);
    heap_end     := (-1);
    heap_mid     := (-1);
    static_start := (-1);
    static_end   := (-1);
    stack_start  := (-1);
    stack_end    := (-1);
    sp           := (-1);
    hp           := (-1);
    pc           := (-1)
  ;;

  let fwd_addr ptr = as_pointer !mem.(ptr)

  let copy_and_fwd ptr nh_top =
    as_int !mem.(ptr)
    >>| fun len ->
    let _ = Array.blit !mem ptr !mem !nh_top len in
    let _ = Array.set !mem ptr (from_int_as_ptr ptr) in
    let hold = !nh_top in
    let _ = nh_top := !nh_top + len in
    hold
  ;;

  let handle_heap_ptr ptr nh_top = fwd_addr ptr <|> copy_and_fwd ptr nh_top

  let garbage_collect () =
    let old_heap = if !hp > !heap_mid then heap_mid else heap_start in
    let old_end = !old_heap + (!heap_mid - !heap_start) in
    let new_heap = if old_heap == heap_mid then heap_start else heap_mid in
    let new_heap_next = ref !new_heap in
    let on_old_heap p = p >= !old_heap && p < old_end in
    let maybe_copy_slot i =
      value
        (let* p = as_pointer !mem.(i) in
         (* is a ptr *)
         let* p =
           if on_old_heap p (* is on old heap *)
           then some p
           else none
         in
         let* np = handle_heap_ptr p new_heap_next in
         (* copy or follow fwd *)
         return (Array.set !mem i (from_int_as_ptr np))
         (* update the old ptr *))
        ~default:() (* collapse unit option to unit *)
    in
    for i = !sp to !stack_start do
      maybe_copy_slot i
    done;
    (* copy initial heap contents from stack *)
    let i = ref !new_heap in
    while !i < !new_heap_next do
      maybe_copy_slot !i;
      i := !i + 1
    done;
    (* traverse new heap, copying pointers from old to new causing growth *)
    hp := !new_heap_next (* finalize, swap to new heap *)
  ;;

  let bump_maybe_gc n =
    if !hp < !heap_mid && !hp + n + 1 >= !heap_mid
    then (
      let _ = garbage_collect () in
      if !hp + n + 1 >= !heap_end
      then raise (Failure "OOM")
      else (
        let h = !hp + 1 in
        let _ = hp := h + n in
        h))
    else if !hp + n + 1 >= !heap_end
    then (
      let _ = garbage_collect () in
      if !hp + n + 1 >= !heap_mid
      then raise (Failure "OOM")
      else (
        let h = !hp + 1 in
        let _ = hp := h + n in
        h))
    else (
      let h = !hp + 1 in
      let _ = hp := h + n in
      h)
  ;;

  let run1 () =
    let pop () = sp := !sp + 1 in
    let push s =
      !mem.(!sp) <- s;
      sp := !sp - 1
    in
    let inc_pc () = pc := !pc + 1 in
    let op =
      match !mem.(!pc) with
      | Num _ -> raise (Failure "Tried to execute something other than an opcode")
      | Op o -> o
    in
    match op with
    | PushLit l ->
      push (from_int_as_num l);
      inc_pc ()
    | ResStack s ->
      sp := !sp - s;
      inc_pc ()
    | FetchSp n ->
      let v = !mem.(!sp + n + 1) in
      push v;
      inc_pc ()
    | SetSp n ->
      let x = !mem.(!sp + 1) in
      !mem.(!sp + 1 + n) <- x;
      pop ();
      inc_pc ()
    | Swap ->
      let hold = !mem.(!sp + 1) in
      !mem.(!sp + 1) <- !mem.(!sp + 2);
      !mem.(!sp + 2) <- hold;
      inc_pc ()
    | Drop n ->
      sp := !sp + n;
      inc_pc ()
    | Alloc n ->
      let h = bump_maybe_gc n in
      push (from_int_as_ptr h);
      inc_pc ()
    | Fetch n ->
      let x =
        Option.value
          (as_pointer !mem.(!sp + 1))
          ~default:(raise (Failure "Address of fetch wasn't a pointer"))
      in
      !mem.(!sp + 1) <- !mem.(x + n);
      inc_pc ()
    | FetchRegion n ->
      let x =
        Option.value
          (as_pointer !mem.(!sp + 2))
          ~default:(raise (Failure "Address of fetch region wasn't a pointer"))
      in
      let y =
        Option.value
          (as_int !mem.(!sp + 1))
          ~default:(raise (Failure "Size of fetch region wasn't a number"))
      in
      pop ();
      for i = x + n to x + n + y do
        push !mem.(i)
      done;
      inc_pc ()
    | FetchSize (x, n) ->
      for i = x to x + n do
        push !mem.(i)
      done;
      inc_pc ()
    | Set n ->
      let v = !mem.(!sp + 1) in
      let x =
        Option.value
          (as_pointer !mem.(!sp + 2))
          ~default:(raise (Failure "Address of set wasn't a pointer"))
      in
      sp := !sp + 2;
      !mem.(x + n) <- v;
      inc_pc ()
    | Call ->
      let x =
        Option.value
          (as_int !mem.(!sp + 1))
          ~default:(raise (Failure "Address of call wasn't a pointer"))
      in
      inc_pc ();
      !mem.(!sp + 1) <- from_int_as_ptr !pc;
      pc := x
    | Ret ->
      let x =
        Option.value
          (as_int !mem.(!sp + 1))
          ~default:(raise (Failure "Address of return wasn't a pointer"))
      in
      pop ();
      pc := x
    | Jump n -> pc := !pc + n
  ;;
end
