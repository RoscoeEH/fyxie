
type name_atom
type name

val separator : char
val pp_name : name -> string
val pp_name_atom : name_atom -> string
val name_of_string : string -> (name, string) Result.t
val name_atom_of_string : string -> (name_atom, string) Result.t
val compare : name -> name -> int

val add_prefix : name_atom -> name -> (name, string) Result.t
val drop_prefix : name -> (name, string) Result.t
