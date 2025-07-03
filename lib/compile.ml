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
