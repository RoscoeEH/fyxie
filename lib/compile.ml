(*
   * The idea for this module is to do the CST -> IR transformation.
 *
 *
*)

open Ast
open Dynarray
open Bytecode

(* delayed computation monad to track stack effects and code offsets *)

module type CompTracker = sig
  type 'a t
  type op

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val emit : op -> unit t
  val stack_offset : int t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val seq : unit t -> 'b t -> 'b t
  val ( >> ) : unit t -> 'b t -> 'b t
  val out_of_line : 'b t -> ('b * int) t
  val run_empty : 'a t -> op Dynarray.t
end

module Tracker : CompTracker with type op := BC.op = struct
  type ctx =
    { code_offset : int
    ; stack_offset : int
    ; target : BC.op Dynarray.t
    }

  type 'a t = { run : ctx -> 'a * ctx }

  let run_empty action =
    let empty_ctx = { code_offset = 0; stack_offset = 0; target = Dynarray.create () } in
    (snd (action.run empty_ctx)).target
  ;;

  let return a =
    let run = fun ctx -> a, ctx in
    { run }
  ;;

  let bind a f =
    let run =
      fun ctx ->
      let v, ctx2 = a.run ctx in
      (f v).run ctx2
    in
    { run }
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let seq a b =
    let* _ = a in
    b
  ;;

  let ( >> ) = seq

  let emit op =
    let run =
      fun ctx ->
      add_last ctx.target op;
      let s_offset = Option.value (BC.stack_effect op) ~default:0 in
      (* TODO the offset can fail for fetch_region, but this is unavoidable.
         * This is only useful/required for fetching closures on application
         * and the size can't be known at compile time, so either a fetch of
         * unknown size, or a loop with unknown iterations needs to happen.
         *
         * Either way, the count of unknown. I am choosing to somewhat break
         * the abstraction here rather than forgo it entirely, since I
         * think it is useful otherwise and this has limited scope. *)
      let ctx2 =
        { code_offset = ctx.code_offset + 1
        ; stack_offset = ctx.stack_offset + s_offset
        ; target = ctx.target
        }
      in
      (), ctx2
    in
    { run }
  ;;

  let stack_offset =
    let run = fun ctx -> ctx.stack_offset, ctx in
    { run }
  ;;

  (* implicitly assumes there will be an emitted instruction after
   * the out of line block that will be jumped to by the current
   * computation line. *)
  let out_of_line m =
    let run =
      fun ctx ->
      let inner_target = Dynarray.create () in
      let inner_ctx =
        { code_offset = ctx.code_offset + 1; stack_offset = 0; target = inner_target }
      in
      let v, inner_ctx2 = m.run inner_ctx in
      let inner_ptr = length ctx.target + ctx.code_offset in
      let next_ptr = ctx.code_offset + length inner_ctx2.target in
      (emit (BC.jump next_ptr)
       >>= fun () ->
       append ctx.target inner_target;
       return (v, inner_ptr))
        .run
        ctx
    in
    { run }
  ;;
end

module Compiler : sig
  type 'a t

  val run_empty : 'a t -> BC.op Dynarray.t
  val compile : Ast.expr -> unit t
end = struct
  (* conventions:
   * Every expresion, when evaluated, leaves a single new value on the stack
   *
   * literals just add a value
   * variable references fetch a stack slot
   * functions capture values and leave the heap pointer to the closure
   * let blocks alloc slot space for locals, initialize them, and then eval body, cleaning up after
   *
   * function application does the following:
   * reserves 2 slots
   * evaluates the args in reverse order
   * populates the stack with the captured values from the closure
   * calls the function
   *
   * The function boilerplate is responsible for:
   * moving the return addr to the bottom reserved slot
   * executing the body
   * moving the return value to the top reserved slot
   * dropping the args and the captured values
   * returning, leaving the return value on TOS
   *
   * function closures are pointers to heap allocations with the follwing layout:
   * 1 slot containing the size of the rest of the closure as an integer
   * N slots for the N closure values
   * code ptr
  *)
  open BC
  open Tracker

  type 'a t = 'a Tracker.t

  let run_empty = run_empty

  (* helpers for indexed monadic folds *)
  let fold_left_mi (f : int -> 'a -> 'b -> 'b t) (acc : 'b t) (arr : 'a Array.t) =
    let rec helper f i acc =
      if i == Array.length arr
      then acc
      else (
        let acc2 = acc >>= f i arr.(i) in
        helper f (i + 1) acc2)
    in
    helper f 0 acc
  ;;

  let fold_right_mi (f : int -> 'b -> 'a -> 'b t) (acc : 'b t) (arr : 'a Array.t) =
    let rec helper f i acc =
      if i < 0
      then acc
      else (
        let acc2 = acc >>= fun x -> f i x arr.(i) in
        helper f (i - 1) acc2)
    in
    helper f (Array.length arr - 1) acc
  ;;

  let rec compile expr =
    match expr.inner with
    | Lit l -> emit (push_lit l.value)
    | Var v ->
      let* rt_off = stack_offset in
      emit (fetch_stack (v.v_slot + rt_off))
    | Let l ->
      let* rt_off_prior = stack_offset in
      let arm_helper _i _acc (_bind, arm) = compile arm in
      fold_right_mi arm_helper (return ()) l.binds
      >> compile l.l_body
      >>
      let* rt_off_after = stack_offset in
      let diff = rt_off_after - rt_off_prior in
      emit (set_stack_x diff) >> emit (drop diff)
    | Fun f ->
      let n_args = Array.length f.f_args in
      let n_caps = Array.length f.captures in
      let closure_size = n_caps + 1 in
      emit (alloc (1 + closure_size))
      >> emit (push_lit closure_size)
      >> emit (fetch_stack 1)
      >> emit (set_x_y 0)
      >>
      (* wrote the closure size to heap *)
      let* ptr_off = stack_offset in
      let populate_captures i (_, origin_slot) _acc =
        let* off = stack_offset in
        emit (fetch_stack (off - ptr_off))
        >>
        (* copied closure ptr to top *)
        let* off = stack_offset in
        emit (fetch_stack (off + origin_slot))
        >>
        (* copied closure val to top *)
        emit (set_x_y (i + 1))
        >>
        (* wrote captured val to heap closure *)
        return ()
      in
      fold_left_mi populate_captures (return ()) f.captures
      >>
      let func_body_with_boilerplate =
        emit (set_stack_x (n_caps + n_args + 1))
        >>
        (* wrote the return addr back above the args and captures,
         * reserved by caller *)
        compile f.f_body
        >>
        (* the good stuff *)
        emit (set_stack_x (n_caps + n_args + 2))
        >>
        (* wrote the return value back above, same deal *)
        emit (drop (n_caps + n_args))
        >> emit return_x
      in
      let* _, code_ptr = out_of_line func_body_with_boilerplate in
      emit (fetch_stack 0)
      >>
      (* dup'd closure ptr *)
      emit (push_lit code_ptr)
      >>
      (* pushed func body code ptr to stack *)
      emit (set_x_y n_caps)
    (* wrote code ptr to closure *)
    | App a ->
      let arg_helper _i arg _acc = compile arg in
      emit (reserve_stack 2)
      >> fold_left_mi arg_helper (return ()) a.a_args
      >>
      (* wrote args to stack, with first arg at TOS *)
      compile a.func
      >>
      (* wrote closure ptr to stack *)
      emit (fetch_stack 0)
      >> emit (fetch_x 0)
      >>
      (* fetched closure size to TOS *)
      emit (fetch_region_x_y 1)
      >>
      (* copied rest of closure to stack *)
      emit call_x
  ;;
end
