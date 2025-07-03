(* TODO interface? *)

(* CT = compile time *)
(* RT = runtime *)
(* TOS = top of stack *)
(* capital letters are CT values *)
  (*
   * type bc_op
   * type slot
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
  type 'a ctx

  (*
   * val to_opcode : slot -> bc_op option
   * val to_slot : bc_op -> slot
   *
   * val is_pointer : slot -> bool
   * val as_pointer : slot -> int option
   * val zero : slot
   * val as_int : slot -> int option
   *)

  (* codes *)
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
   * val push_lit : int -> bc_op ctx      (\* place a CT value on the RT stack *\)
   * val reserve_stack : int -> bc_op ctx (\* reserve N new slots on the RT stack *\)
   * val fetch_stack : int -> bc_op ctx   (\* fetch the Nth slot on the stack, with 0 being TOS *\)
   * val set_stack_x : int -> bc_op ctx   (\* set the Nth slot on the stack to val from TOS, popping it *\)
   * val dup_x : bc_op ctx                (\* push another copy of TOS *\)
   * val alloc : int -> bc_op ctx         (\* push x, a pointer to a new heap space with N slots *\)
   * val fetch_x : int -> bc_op ctx       (\* push v, the value obtained by fetching x from TOS with N slots offset *\)
   * val set_x_y : int -> bc_op ctx       (\* set value at x (deeper) plus N slots offset to y (TOS) *\)
   * val call_x : bc_op ctx               (\* transfer control to x (TOS), replacing it with the location of the next slot that would have been executed *\)
   * val return_x : bc_op ctx             (\* transfer control to x (TOS), popping it *\)
   *)
