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
 * *)


let compile target expr offset =
  match expr with
  | Lit l ->
    add_last target (PushLit l.value);
    offset := !offset + 1
  | Var v ->
    add_last target (FetchSp (v.v_slot + !offset));
    offset := !offset + 1
   | Fun _ -> _
  | Let _ -> _
  | App _ -> _





