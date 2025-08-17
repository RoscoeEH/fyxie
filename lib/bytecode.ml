(*
   Define stack machine ISA and some helpers
*)

open Option

(* NOTE we can't be using the module sig cause it hides the slot
   constructors. Later if we want to actually compile to real opcodes,
   we should add another step later. *)
module BC = struct
  (* basic types that implement bytecode. Suitable for interpretation. *)
  type slot =
    | Num of int
    | Op of op (* [@warning "-37"] *)
        
  and op =
    | PushLit of slot
    | ResStack of int
    | FetchSp of int
    | SetSp of int
    | Swap
    | Drop of int
    | Alloc of int
    | Fetch of int
    | FetchRegion of int
    | FetchSize of int * int
    | Set of int
    | Call
    | Ret
    | Jump of int
  ;;

  let stack_effect op =
    match op with
    | PushLit _ -> some 1
    | ResStack i -> some i
    | FetchSp _ -> some 1
    | SetSp _ -> some (-1)
    | Swap -> some 0
    | Drop i -> some i
    | Alloc _ -> some 1
    | Fetch _ -> some 0
    | FetchRegion _ -> none
    | FetchSize (_,s) -> some s
    | Set _ -> some (-2)
    | Call -> some 0
    | Ret -> some (-1)
    | Jump _ -> some 0
  ;;

  let is_pointer s =
    match s with
    | Op _ -> false
    | Num i -> Int.logand i 1 = 1
  ;;

  let as_pointer s =
    match s with
    | Op _ -> none
    | Num i -> if Int.logand i 1 = 1 then some (Int.shift_right i 1) else none
  ;;

  let as_int s =
    match s with
    | Op _ -> none
    | Num i -> if Int.logand i 1 = 1 then none else some (Int.shift_right i 1)
  ;;

  let zero = Num 0
  let from_int_as_num i = Num (Int.shift_left i 1)
  let from_int_as_ptr i = Num (Int.logor (Int.shift_left i 1) 1)
  let from_int_literal i = Num i

  let rec pp_op o =
    match o with
    | PushLit i -> "pushlit " ^ pp_slot i
    | ResStack i -> "ressp " ^ string_of_int i
    | FetchSp i -> "fetchsp " ^ string_of_int i
    | SetSp i -> "setsp " ^ string_of_int i
    | Swap -> "swap"
    | Drop i -> "drop " ^ string_of_int i
    | Alloc i -> "alloc " ^ string_of_int i
    | Fetch i -> "fetch " ^ string_of_int i
    | FetchRegion n -> "fetchmany " ^ string_of_int n
    | FetchSize (i,s) -> "fetchsize " ^ string_of_int i ^","^ string_of_int s
    | Set i -> "set " ^ string_of_int i
    | Call -> "call"
    | Ret -> "return"
    | Jump i -> "jump " ^ (if i >= 0 then "+" else "") ^ string_of_int i

  and pp_slot s =
    match s with
    | Num i ->
      let x = string_of_int @@ Int.shift_right i 1 in
      if is_pointer s
      then "ptr: " ^ x
      else "int: " ^ x 
    | Op o -> pp_op o
  ;;

  (* codes *)
  (* stack indicies are calculated before the stack effect of the opcode. *)
  (* place a CT value on the RT stack ( -- x ) *)
  let push_lit ?(is_ptr = false) i =
    if is_ptr
    then PushLit (from_int_as_ptr i)
    else PushLit (from_int_as_num i)
  ;;

  (* reserve N new slots on the RT stack ( -- x_1 ... x_n ) *)
  let reserve_stack n = ResStack n

  (* fetch the Nth slot on the stack, with 0 being TOS ( -- x ) *)
  let fetch_stack n = FetchSp n

  (* set the Nth slot on the stack to val from TOS, popping it ( x -- ) *)
  let set_stack_x n = SetSp n

  (* Swap the top stack elements ( x y -- y x ) *)
  let swap = Swap

  (* drop the top N slots of the stack, inverse of reserve_stack *)
  let drop n = Drop n

  (* push x, a pointer to a new heap space with N slots ( -- x ) *)
  let alloc n = Alloc n

  (* NOTE: Non stack fetches range over both the heap and the static space. *)
  
  (* push v, the value obtained by fetching x from TOS with N slots offset ( x -- y ) *)
  let fetch_x n = Fetch n

  (* push v..v_(y-1), the value obtained by fetching x with N slots
   * offset, followed by x+N+1 up to x+N+(y-1) ( x y -- v..v_(y-1)) *)
  let fetch_region_x_y n = FetchRegion n

  (* fetch the i'th through (i+s-1)'th slot onto the stack  *)
  let fetch_size i s = FetchSize (i, s)
  
  (* set value at x (deeper) plus N slots offset to y (TOS) ( x y -- ) *)
  let set_x_y n = Set n

  (* transfer control to x (TOS), replacing it with the location of
   * the next slot that would have been executed ( x -- r ) *)
  let call_x = Call

  (* transfer control to r (TOS), popping it ( r -- ) *)
  let return_x = Ret

  (* transfer control to an offset calculated from the next slot to be executed.
   * Stack untouched ( -- ) *)
  let jump n = Jump n
end
