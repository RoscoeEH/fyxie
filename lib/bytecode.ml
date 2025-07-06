(* TODO interface? *)
open Option

module type Bytecode = sig
  type bc_op =
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
    | Op of bc_op
  type 'a ctx

  (* effect of executing instruction on the size of the stack *)
  val stack_effect : bc_op ctx -> int option

  (* stack indicies are calculated before the stack effect of the opcode. *)

  (* codes *)
  (* place a CT value on the RT stack ( -- x ) *)
  val push_lit : int -> bc_op ctx
  (* reserve N new slots on the RT stack ( -- x_1 ... x_n ) *)
  val reserve_stack : int -> bc_op ctx
  (* fetch the Nth slot on the stack, with 0 being TOS ( -- x ) *)
  val fetch_stack : int -> bc_op ctx
  (* set the Nth slot on the stack to val from TOS, popping it ( x -- ) *)
  val set_stack_x : int -> bc_op ctx
  (* Swap the top stack elements ( x y -- y x ) *)
  val swap : bc_op ctx
  (* drop the top N slots of the stack, inverse of reserve_stack *)
  val drop : int -> bc_op ctx
  (* push x, a pointer to a new heap space with N slots ( -- x ) *)
  val alloc : int -> bc_op ctx
  (* push v, the value obtained by fetching x from TOS with N slots offset ( x -- y ) *)
  val fetch_x : int -> bc_op ctx
  (* push v..v_(M-1), the value obtained by fetching x with N slots
   * offset, followed by x+N+1 up to x+N+(y-1) ( x y -- v..v_(y-1)) *)
  val fetch_region_x_y : int -> bc_op ctx
  (* set value at x (deeper) plus N slots offset to y (TOS) ( x y -- ) *)
  val set_x_y : int -> bc_op ctx
  (* transfer control to x (TOS), replacing it with the location of the next slot that would have been executed ( x -- r ) *)
  val call_x : bc_op ctx
  (* transfer control to r (TOS), popping it ( r -- ) *)
  val return_x : bc_op ctx
  (* transfer control to an offset calculated from the next slot to be executed. Stack untouched ( -- ) *)
  val jump : int -> bc_op ctx
end

(* CT = compile time *)
(* RT = runtime *)
(* TOS = top of stack *)
(* capital letters are CT values *)
module BC : Bytecode = struct
    type bc_op =
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
    | Op of bc_op
  type 'a ctx = 'a

  let stack_effect op = match op with
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
