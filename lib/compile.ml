(*
 * The idea for this module is to do the CST -> IR transformation. The
 * output should be something like an array of the IR opcodes.
 *
 * TODO the interface between the IR and the rest of the stack is not
 * really clear to me. The main thing is that it can't be a complete
 * black box module, since things like the ocml interpretor need to be
 * able to see into some of the types, but it would also be nice to
 * have the IR output be abstracted over the internal details of the
 * bytecode.
 *)

open Ast
open Interpret.Interpreter
open Dynarray

(* conventions:
 * Every expresion, when evaluated, leaves a single new value on the stack
 *
 * literals just add a value
 * variable references fetch a stack slot
 * functions capture values and leave the heap pointer to the closure
 * let blocks alloc slot space for locals, initialize them, and then eval body, cleaning up after
 *
 * function application reserves a slot, then evaluates the args in reverse order, then calls the function
 * At the end of the function, the return value is written to the reserved slot, the locals are dropped, and the return is executed
 *
 * function closures are pointers to heap allocations with the follwing layout:
 *
 * *)

let rec compile target expr offset =
  let emit op = add_last target op in
  let inc_off = offset := !offset + 1 in
  match expr.inner with
  | Lit l ->
    emit (push_lit l.value);
    inc_off
  | Var v ->
    emit (fetch_stack (v.v_slot + !offset));
    inc_off
  | Let l ->
    let hold_off = !offset in
    let arm_helper (_bind, arm) = compile target arm offset in
    Array.iter arm_helper l.binds; (* TODO reverse? *)
    compile target l.l_body offset;
    emit (set_stack_x (!offset - hold_off));
    emit (drop (!offset - hold_off));
    offset := hold_off + 1 (* TODO I'm suspicious about these offsets *)
  | Fun f ->
    let n_args = Array.length f.f_args in
    let n_caps = Array.length f.captures in
    let closure_size = n_args + n_caps + 1 in
    emit (alloc closure_size);
    inc_off;
    let ptr_off = !offset in
    for i = 0 to Array.length f.captures do
      let (_, from_slot) = f.captures.(i) in
      emit (fetch_stack (!offset - ptr_off)); (* copy closure ptr to top *)
      emit (fetch_stack (!offset + from_slot + 1)); (* copy closure val to top *)
      emit (set_x_y (1 + n_args + i))
    done; (* closures populated *)
    let here = length target in (* TODO right way to get code ptr? *)
    let body = Dynarray.create () in
    let body_off = ref 0 in
    compile body f.f_body body_off;
    emit (jump (length body));  (* compile fun def inline and a jump over it *)
    append target body;         (* simple compilation, dubious runtime *)
    emit (fetch_stack 0);
    emit (push_lit here);
    emit (set_x_y 0);           (* write code ptr to closure *)
    inc_off                     (* closure ptr on stack *)
  | App _ -> _
    (* TODO see note in ast application about partial application *)
    (*
     * emit (PushLit 0);
     * inc_off;          (\* ret val slot *\)
     * compile target a.func offset;
     *
     * for i = Array.length a.a_args - 1 downto 0 do
     *   compile target a.a_args.(i) offset
     * done;
     *)






