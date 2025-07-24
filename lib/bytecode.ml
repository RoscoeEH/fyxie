(*
   Define stack machine ISA and some helpers
*)

open Option

module type Bytecode = sig
  type op
  type slot
    
  (* effect of executing instruction on the size of the stack *)
  val stack_effect : op -> int option
      
  val pretty_print_op : op -> string
  val pretty_print_slot : slot -> string

  val is_pointer : slot -> bool
  val as_pointer : slot -> int option
  val zero : slot
  val from_int_as_num : int -> slot
  val from_int_as_ptr : int -> slot
    
  (* codes *)
  (* stack indicies are calculated before the stack effect of the opcode. *)
  (* place a CT value on the RT stack ( -- x ) *)
  val push_lit : int -> op 

  (* reserve N new slots on the RT stack ( -- x_1 ... x_n ) *)
  val reserve_stack : int -> op 

  (* fetch the Nth slot on the stack, with 0 being TOS ( -- x ) *)
  val fetch_stack : int -> op 

  (* set the Nth slot on the stack to val from TOS, popping it ( x -- ) *)
  val set_stack_x : int -> op 

  (* Swap the top stack elements ( x y -- y x ) *)
  val swap : op 

  (* drop the top N slots of the stack, inverse of reserve_stack *)
  val drop : int -> op 

  (* push x, a pointer to a new heap space with N slots ( -- x ) *)
  val alloc : int -> op 

  (* push v, the value obtained by fetching x from TOS with N slots offset ( x -- y ) *)
  val fetch_x : int -> op 

  (* push v..v_(y-1), the value obtained by fetching x with N slots
   * offset, followed by x+N+1 up to x+N+(y-1) ( x y -- v..v_(y-1)) *)
  val fetch_region_x_y : int -> op 

  (* set value at x (deeper) plus N slots offset to y (TOS) ( x y -- ) *)
  val set_x_y : int -> op 

  (* transfer control to x (TOS), replacing it with the location of the next slot that would have been executed ( x -- r ) *)
  val call_x : op 

  (* transfer control to r (TOS), popping it ( r -- ) *)
  val return_x : op 

  (* transfer control to an offset calculated from the next slot to be executed. Stack untouched ( -- ) *)
  val jump : int -> op 
end

module BC : Bytecode = struct
  (* basic types that implement bytecode. Suitable for interpretation. *)
  type op =
    | PushLit of int
    | ResStack of int
    | FetchSp of int
    | SetSp of int
    | Swap
    | Drop of int
    | Alloc of int
    | Fetch of int
    | FetchRegion of int
    | Set of int
    | Call
    | Ret
    | Jump of int

  type slot =
    | Num of int
    | Op of op

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
    | Set _ -> some (-2)
    | Call -> some 0
    | Ret -> some (-1)
    | Jump _ -> some 0
  ;;


  let is_pointer s = match s with
    | Op _ -> false
    | Num i -> Int.logand i 1 == 1
  ;;

  let as_pointer s = match s with
    | Op _ -> none
    | Num i -> if Int.logand i 1 == 1
      then some(Int.shift_right i 1)
      else none
  ;;

  let zero = Num 0

  let from_int_as_num i = Num (Int.shift_left i 1)
  let from_int_as_ptr i = Num (Int.logor (Int.shift_left i 1) 1)
  
  let pretty_print_op o =
    match o with
    | PushLit i -> "pushlit " ^ string_of_int i
    | ResStack i -> "ressp " ^ string_of_int i
    | FetchSp i -> "fetchsp " ^ string_of_int i
    | SetSp i -> "setsp " ^ string_of_int i
    | Swap -> "swap"
    | Drop i -> "drop " ^ string_of_int i
    | Alloc i -> "alloc " ^ string_of_int i
    | Fetch i -> "fetch " ^ string_of_int i
    | FetchRegion n -> "fetchmany " ^ string_of_int n
    | Set i -> "set " ^ string_of_int i
    | Call -> "call"
    | Ret -> "return"
    | Jump i -> "jump " ^ string_of_int i
  ;;

  let pretty_print_slot s =
    match s with
    | Num i -> string_of_int i
    | Op o -> pretty_print_op o
  ;;

  let push_lit n = PushLit n
  let reserve_stack n = ResStack n
  let fetch_stack n = FetchSp n
  let set_stack_x n = SetSp n
  let swap = Swap
  let drop n = Drop n
  let alloc n = Alloc n
  let fetch_x n = Fetch n
  let fetch_region_x_y n = FetchRegion n
  let set_x_y n = Set n
  let call_x = Call
  let return_x = Ret
  let jump n = Jump n
end
