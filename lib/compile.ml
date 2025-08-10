(*
   * The idea for this module is to do the CST -> IR transformation.
 *
 *
*)

open Ir
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
  val adjust_stack_offset : int -> unit t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val enclosing_func : Ir.func option t
  val out_of_line : ?func:Ir.func -> 'b t -> ('b * int) t
  val run_empty : 'a t -> op Dynarray.t
end

module Tracker : CompTracker with type op := BC.op = struct
  type ctx =
    { code_offset : int
    ; stack_offset : int
    ; target : BC.op Dynarray.t
    ; statics : BC.slot Dynarray.t
    ; static_map : (int * (int * int)) list
    ; in_func : func option
    }

  type 'a t = ctx -> 'a * ctx

  let run_empty action =
    let empty_ctx =
      { code_offset = 0
      ; stack_offset = 0
      ; target = Dynarray.create ()
      ; statics = Dynarray.create ()
      ; static_map = []
      ; in_func=None
      }
    in
    (snd (action  empty_ctx)).target
  ;;

  let return a ctx = a, ctx 

  let bind a f ctx = 
    let v, ctx2 = a ctx in
    f v ctx2
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let emit op = fun ctx ->
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
      ; statics = ctx.statics
      ; static_map = ctx.static_map
      ; in_func = ctx.in_func
      }
    in
    (), ctx2
  ;;

  let stack_offset ctx = ctx.stack_offset, ctx 

  let adjust_stack_offset i ctx =
    (), { code_offset=ctx.code_offset
        ; stack_offset=ctx.stack_offset + i
        ; target = ctx.target
        ; statics = ctx.statics
        ; static_map = ctx.static_map
        ; in_func = ctx.in_func
        }
  ;;

  let enclosing_func ctx = ctx.in_func, ctx 
  
  (* implicitly assumes there will be an emitted instruction after
   * the out of line block that will be jumped to by the current
   * computation line. *)
  let out_of_line ?func m = fun ctx ->
    let inner_target = Dynarray.create () in
    let inner_ctx =
      { code_offset = ctx.code_offset + 1
      ; stack_offset = 0
      ; target = inner_target
      ; statics = ctx.statics
      ; static_map = ctx.static_map
      ; in_func=func
      }
    in
    let v, inner_ctx2 = m inner_ctx in
    let inner_ptr = length ctx.target + ctx.code_offset in
    let jump_offset = length inner_ctx2.target in
    (emit (BC.jump jump_offset)
     >>= fun () ->
     append ctx.target inner_target;
     return (v, inner_ptr)
    ) ctx
  ;;
end

module Compiler : sig
  type 'a t

  val run_empty : 'a t -> BC.op Dynarray.t
  val compile : Ir.expr -> unit t
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
   * evaluates the args in order
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

  (*
   * let fold_right_mi (f : int -> 'b -> 'a -> 'b t) (acc : 'b t) (arr : 'a Array.t) =
   *   let rec helper f i acc =
   *     if i < 0
   *     then acc
   *     else (
   *       let acc2 = acc >>= fun x -> f i x arr.(i) in
   *       helper f (i - 1) acc2)
   *   in
   *   helper f (Array.length arr - 1) acc
   * ;;
   *)

  (* TODO doing this with v_id is really fragile, since we aren't
     super clear w/ the assuptions that that be ordered in any
     way. Once we have a Mid/Low IR, we should have explicit ordering
     there. Or just actually annotate / enforce some ordering on the
     IDs. This one is the natural one (Sort of top to bottom, left to
     right) *)
  let var_index ?in_func v =
    (* basic idea is that the stack offset is the distance from TOS to
       the top of the closest function call, and that the id of a var 
       (with some offset per domain) is basically the distance from 
       the top of the function frame to the variable's slot. There is
       also offsets for the return value and address, but those are handled
       elsewhere. *)
    match v.v_domain with
    | Static -> raise @@ Failure "TODO static refs"
    | Arg ->
      (* args are at the top of the function frame *)
      let* off = stack_offset in
      return @@ off - v.v_id
    | Closure ->
      (* captures are below args *)
      let* off = stack_offset in
      let arg_size_opt = Option.map (fun a -> Array.length a.f_args) in_func in
      let arg_size = Option.value ~default:0 arg_size_opt in
      return @@ off - arg_size - v.v_id
    | Local ->
      (* locals are below both args and captures *)
      let* off = stack_offset in
      let arg_size_opt = Option.map (fun a -> Array.length a.f_args) in_func in
      let arg_size = Option.value ~default:0 arg_size_opt in
      let capture_size_opt = Option.map (fun a -> Array.length a.captures) in_func in
      let capture_size = Option.value ~default:0 capture_size_opt in
      return @@ off - arg_size - capture_size - v.v_id
  ;;
  
  let rec compile expr =
    match expr.inner with
    | Lit l -> emit (push_lit l.value)
    | Var v ->
      let* off = var_index v in
      emit (fetch_stack off)
    | Let l ->
      let* rt_off_prior = stack_offset in
      let arm_helper _i an _acc = compile an.rhs in
      let* () = fold_left_mi arm_helper (return ()) l.binds in
      let* () = compile l.l_body in
      let* rt_off_after = stack_offset in
      let diff = rt_off_after - rt_off_prior in
      let* () = emit (set_stack_x diff) in
      emit (drop diff)
    | Fun f ->
      let n_args = Array.length f.f_args in
      let n_caps = Array.length f.captures in
      let closure_size = n_caps + 1 in
      let* () = emit (alloc (1 + closure_size)) in
      let* () = emit (push_lit closure_size) in
      let* () = emit (fetch_stack 1) in
      let* () = emit (set_x_y 0) in
      (* wrote the closure size to heap *)
      let* ptr_off = stack_offset in
      let populate_captures i v _acc =
        let* off = stack_offset in
        let* () = emit (fetch_stack (off - ptr_off)) in
        (* copied closure ptr to top *)
        let* enc_f = enclosing_func in
        let* v_off = var_index ?in_func:enc_f v in
        let* () = emit (fetch_stack v_off) in
        (* copied closure val to top *)
        let* () = emit (set_x_y (i + 1)) in
        (* wrote captured val to heap closure *)
        return ()
      in
      let* () = fold_left_mi populate_captures (return ()) f.captures in
      let func_body_with_boilerplate =
        let* () = emit (set_stack_x (n_caps + n_args + 1)) in
        (* wrote the return addr back above the args and captures,
         * reserved by caller *)
        let* () = adjust_stack_offset 1 in
        (* undo stack offset of moving the return addr *)
        let* () = compile f.f_body in
        (* the good stuff *)
        let* () = emit (set_stack_x (n_caps + n_args + 2)) in
        (* wrote the return value back above, same deal *)
        let* () = emit (drop (n_caps + n_args)) in
        emit return_x
      in
      let* _, code_ptr = out_of_line ?func:(Some f) func_body_with_boilerplate in
      let* () = emit (fetch_stack 0) in
      (* dup'd closure ptr *)
      let* () = emit (push_lit code_ptr) in
      (* pushed func body code ptr to stack *)
      emit (set_x_y n_caps)
    (* wrote code ptr to closure *)
    | App a ->
      let arg_helper _i arg _acc = compile arg in
      let* () = emit (reserve_stack 2) in
      let* () = fold_left_mi arg_helper (return ()) a.a_args in
      (* wrote args to stack, with last arg at TOS *)
      let* () = compile a.func in
      (* wrote closure ptr to stack *)
      let* () = emit (fetch_stack 0) in
      let* () = emit (fetch_x 0) in
      (* fetched closure size to TOS *)
      let* () = emit (fetch_region_x_y 1) in
      (* copied rest of closure to stack, consuming c. ptr *)
      emit call_x
  ;;
end
