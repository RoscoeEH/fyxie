(* Erase or annotate information from IR as necessary for desired output. *)

module Interprete = struct
end

module Compile = struct
  include Ir

  (* From compile:
     basic idea is that the stack offset is the distance from TOS to
     the top of the closest function call, and that the id of a var 
     (with some offset per domain) is basically the distance from 
     the top of the function frame to the variable's slot. There is
     also offsets for the return value and address, but those are handled
     elsewhere. *)

  (* From compile:
     TODO doing this with v_id is really fragile, since we aren't
     super clear w/ the assuptions that that be ordered in any
     way. Once we have a Mid/Low IR, we should have explicit ordering
     there. Or just actually annotate / enforce some ordering on the
     IDs. This one is the natural one (Sort of top to bottom, left to
     right) *)

  (* TODO add wrapper / shim types here that enforce a normalization
     scheme on the numbering of variables. Basically we need to map
     variables to the slot index where they will live at runtime, and
     we want that process to be separate from both compilation and IR
     id numbering. The main reason there is so we can do complicated
     transforms on IR and not worry about preserving relative
     ordering, only preserving the mapping of names to ids. *)
end
