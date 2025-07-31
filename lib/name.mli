
type name_atom
type name

val separator : char
val pp_name : name -> string
val name_of_string : string -> (name, string) Result.t
val compare : name -> name -> int
