(* The idea for this module is to do the CST -> IR transformation. *)

open Ir
open Dynarray
open Bytecode


(* TODO fold error handling into this monad *)
module type CompTracker = sig
  (* delayed computation monad to track stack effects and code offsets *)
  type 'a t
  type op
  type slot

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val emit : op -> unit t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val stack_offset : int t
  val adjust_stack_offset : int -> unit t
  val enclosing_func : Ir.func option t
  val dec_static : id:int -> size:int-> unit t
  val def_static : int -> slot Dynarray.t -> unit t
  val ref_static : int -> (int * int) t
  val out_of_line : Ir.func -> 'b t -> ('b * int) t
  val run_empty : ?static_offset:int -> 'a t -> (slot Dynarray.t * op Dynarray.t)
end

module Tracker : CompTracker
  with type op := BC.op
   and type slot := BC.slot = struct
  type ctx =
    { code_offset : int
    ; stack_offset : int
    ; target : BC.op Dynarray.t
    ; statics : BC.slot Dynarray.t
    ; static_map : (int * (int * int)) list
    ; next_static : int
    ; in_func : func option
    }
  ;;

  type 'a t = ctx -> 'a * ctx

  let run_empty ?(static_offset = 0) action =
    let empty_ctx =
      { code_offset = 0
      ; stack_offset = 0
      ; target = Dynarray.create ()
      ; statics = Dynarray.create ()
      ; next_static = static_offset
      ; static_map = []
      ; in_func=None
      }
    in
    let (_,outctx) = action empty_ctx in
    outctx.statics, outctx.target
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
      ; next_static = ctx.next_static
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
        ; next_static = ctx.next_static
        ; in_func = ctx.in_func
        }
  ;;

  let enclosing_func ctx = ctx.in_func, ctx 

  let dec_static ~id ~size ctx =
    let e = id, (ctx.next_static, size) in
    let n_size = ctx.next_static + size in
    let statics = Dynarray.make n_size BC.zero in
    (), { code_offset=ctx.code_offset + size
        ; stack_offset=ctx.stack_offset
        ; target = ctx.target
        ; statics = statics
        ; static_map = e :: ctx.static_map
        ; next_static = ctx.next_static + size
        ; in_func = ctx.in_func
        }
  ;;
  
  let def_static idx contents ctx =
    let len = Dynarray.length contents in
    let prior_dec = List.assoc_opt idx ctx.static_map in
    match prior_dec with
    | None ->
      raise @@ Failure "Def on non-declared static."
    | Some (offset, size) ->
      if size <> len
      then raise @@ Failure "Def and dec static size don't match."
      else
        let updated_statics = ctx.statics in
        for i = 0 to size-1 do
          Dynarray.set updated_statics (i+offset) (Dynarray.get contents i)
        done;
        (), { code_offset=ctx.code_offset
            ; stack_offset=ctx.stack_offset
            ; target = ctx.target
            ; statics = updated_statics
            ; static_map =  ctx.static_map
            ; next_static = ctx.next_static
            ; in_func = ctx.in_func
            }
  ;;

  let ref_static idx ctx = List.assoc idx ctx.static_map, ctx

  (* implicitly assumes there will be an emitted instruction after
   * the out of line block that will be jumped to by the current
   * computation line. *)
  let out_of_line func m = fun ctx ->
    let adjust_code_offset ctx =
      (), { code_offset=ctx.code_offset - 1
          ; stack_offset=ctx.stack_offset
          ; target = ctx.target
          ; statics = ctx.statics
          ; static_map = ctx.static_map
          ; next_static = ctx.next_static
          ; in_func = ctx.in_func
          }
    in
    let inner_target = Dynarray.create () in
    let inner_ctx =
      { code_offset = ctx.code_offset + 1
      ; stack_offset = 0
      ; target = inner_target
      ; statics = ctx.statics
      ; static_map = ctx.static_map
      ; next_static = ctx.next_static
      ; in_func=(Some func)
      }
    in
    let v, inner_ctx2 = m inner_ctx in
    let inner_ptr = length ctx.target + ctx.code_offset + 1 in
    let jump_offset = length inner_ctx2.target in
    ctx |> (let* () = emit (BC.jump jump_offset) in
            append ctx.target inner_target;
            let* () = adjust_code_offset in
            (* NOTE not sure why we need this adjustment. Something about the jump being counted twice? *)
            return (v, inner_ptr))
  ;;
end

module Compiler : sig
  type 'a t = 'a Tracker.t
  val run_empty : ?static_offset:int -> 'a t -> BC.slot Dynarray.t * BC.op Dynarray.t
  val then_stop : 'a t -> 'a t
  val compile : Ir.expr -> unit t
  val compile_top_level : Ir.top_level -> unit t
  val compile_mod : Ir.mod_t -> unit t
  val static_pass_mod : Ir.mod_t -> unit t
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
   *
   * datatypes are represented by fixed size regions of N+1 slots
   * where N is the maximum number of arguments in all the constructors.
   * The first slot is the descriminant, which indicates which constructor was used
   * the remaining slots are the args used for that constructor with possibly empty slots after.
   *
   * datatypes exist on the heap for now
   *
   * a case expression examines the desciminant to select an arm,
   * binds any new variables and evaluates the appropriate arm, similar to a let block.
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

  let var_fetch v =
    (* Idea here is that stack_offset trackes changes inside the body,
     * but you have to do offset for captures and args yourself
     * since they are populated by the caller *)
    let* in_func = enclosing_func in
    let n_args = Option.map (fun f -> Array.length f.f_args) in_func in
    let n_caps = Option.map (fun f -> Array.length f.captures) in_func in

    match v.v_domain with
    | Static ->
      let* offset, size = ref_static v.v_id in
      begin match Types.stack_size v.v_tp with
        | Types.Heap _n -> return @@ push_lit ~is_ptr:true offset
        | Types.Stack n ->
          if size <> n
          then raise @@ Failure "Type and static disagree in size"
          else if size <> 1 
          then raise @@ Failure "Static fetchs of >1 size not supported"
          else return @@ fetch_size offset 1
      end 
    | Arg ->
      (* args are at the top of the function frame *)
      let* off = stack_offset in
      print_endline ("n_args " ^ (string_of_int @@ Option.get n_args)
                     ^ " n_caps " ^ string_of_int (Option.get n_caps)
                    ^ " off " ^ string_of_int off
                    ^ " v_id " ^ string_of_int v.v_id);
      return @@ fetch_stack @@ (Option.get n_args) + (Option.get n_caps) + off - v.v_id - 1
    | Closure ->
      (* captures are below args *)
      let* off = stack_offset in
      return @@ fetch_stack @@ off - (Option.get n_caps) - v.v_id - 1
    | Local ->
      (* locals are below both args and captures *)
      let* off = stack_offset in
      return @@ fetch_stack @@ off - v.v_id
  ;;

  let emit_freeze () = emit @@ jump (-1)
  let then_stop a =
    let* x = a in
    let* () = emit_freeze () in
    return x
  ;;
  
  let rec compile expr =
    match expr.inner with
    | Lit l -> emit @@ push_lit l.value
    | Var v -> var_fetch v >>= emit
    | Let l ->
      let n_binds = Array.length l.binds in
      let arm_helper _i an _acc = compile an.rhs in
      let* () = fold_left_mi arm_helper (return ()) l.binds in
      let* () = compile l.l_body in
      let* () = emit (set_stack_x n_binds) in
      emit (drop (n_binds - 1))
    | Fun f ->
      let n_caps = Array.length f.captures in
      let* () = emit (alloc (2 + n_caps)) in
      let* () = emit (fetch_stack 0) in
      let* () = emit (push_lit (1+n_caps)) in
      let* () = emit (set_x_y 0) in                             (* wrote the closure size to heap *)
      let* ptr_off = stack_offset in
      let populate_captures i v _acc =
        let* off = stack_offset in
        let* () = emit (fetch_stack (off - ptr_off)) in         (* copied closure ptr to top *)
        let* var = var_fetch v in
        let* () = emit var in                                   (* copied closure val to top *)
        let* () = emit (set_x_y (i + 1)) in                     (* wrote captured val to heap closure *)
        return ()
      in
      let* () = fold_left_mi populate_captures (return ()) f.captures in
      let* _, code_ptr = out_of_line f (func_body_with_boilerplate f) in
      let* () = emit (fetch_stack 0) in                         (* dup'd closure ptr *)
      let* () = emit (push_lit code_ptr) in                     (* pushed func body code ptr to stack *)
      emit (set_x_y (n_caps+1))                                 (* wrote code ptr to closure *)
    | App a ->
      let arg_helper _i arg _acc = compile arg in
      let* () = emit (reserve_stack 2) in
      let* () = fold_left_mi arg_helper (return ()) a.a_args in (* wrote args to stack, with last arg at TOS *)
      let* () = compile a.func in                               (* wrote closure ptr to stack *)
      let* () = emit (fetch_stack 0) in
      let* () = emit (fetch_x 0) in                             (* fetched closure size to TOS *)
      let* () = emit (fetch_region_x_y 1) in                    (* copied rest of closure to stack,
                                                                 * consuming c. ptr *)
      emit call_x
    | Dec _c ->
      raise @@ Failure "TODO compile case"
      
        
  and func_body_with_boilerplate f =
    let n_args = Array.length f.f_args in
    let n_caps = Array.length f.captures in
    let* () = emit (set_stack_x (n_caps + n_args + 1)) in       (* wrote the return addr back
                                                                 * above the args and captures,
                                                                 * reserved by caller *)
    let* () = adjust_stack_offset 1 in                          (* undo stack offset of moving the return addr *)
    let* () = compile f.f_body in                               (* the good stuff *)
    let* () = emit (set_stack_x (n_caps + n_args + 2)) in       (* wrote the return value back above, same deal *)
    let* () = emit (drop (n_caps + n_args)) in
    emit return_x
  ;;

  let tl_an_rhs_value buf a =
    (* only safe on top level assignments *)
    if not @@ Ir.refs_allow_static a.rhs
    then raise @@ Failure 
        (Ir.PrettyPrint.pp_expr a.rhs
         ^ " not allowed as a static. Does it have captured variables or runtime computation?\n")
    else 
    match a.rhs.inner with
    | Lit l ->
      let () = Dynarray.add_last buf @@ BC.from_int_as_num l.value in
      return ()
    | Var _ ->
      raise @@ Failure "RHS variable not allowed at toplevel" 
    | Fun f -> 
      let () = Dynarray.add_last buf @@ BC.from_int_as_num 1 in
      let* _, code_ptr = out_of_line f (func_body_with_boilerplate f) in
      let () = Dynarray.add_last buf @@ BC.from_int_as_num code_ptr in
      return ()
    | _ -> raise @@ Failure "This should get caught by possible_static above"
  ;;
  
  let rec compile_top_level tl =
    match tl with
    | TL_ex e -> compile e
    | TL_an a ->
      let buf = Dynarray.create () in
      let* () = tl_an_rhs_value buf a in
      def_static a.lhs.v_id buf
    | TL_m m -> compile_mod m
    | TL_dt _d -> raise @@ Failure "TODO datatype"

  and compile_mod m =
    let seq acc tl = acc >>= fun _ -> compile_top_level tl in
    Array.fold_left seq (return ()) m.top
  ;;

  let rec static_pass_mod m =
    let helper acc tl =
      match tl with
      | TL_an a ->
        let id = a.lhs.v_id in
        let size = match Types.stack_size a.lhs.v_tp with
          | Heap n -> n
          | Stack n -> n
        in
        let* x = acc in
        let* () = dec_static ~id ~size in
        return x
      | TL_m sm ->
        let* x = acc in
        let* () = static_pass_mod sm in
        return x
      | _ -> acc
    in
    Array.fold_left helper (return ()) m.top
end
