# Tentative structure #

# Passes / Phases: #
  * tokenizer/lexer: Character stream -> token stream
  * parser: token stream -> Concrete syntax tree
  * (cst -> abstract syntax tree if needed)
  * Ast -> IR / bytecode
  * Ir/bytecode assembler (or interpreter)

# Types #
  * only primitive type is Int
  * Type names start with a capital letter
  * Arity checking on function application
  * Early extension: Type checking when there is more than 1 possible non-function type.
  * Early extension: Datatypes
  (finite constructors, each with a 0+ input types)
  (can define unit, etc with these)
  * Early extension: Type aliases
  (maps immediately to the target type, shortens name etc)
  * Later extension: Type arguments

# Syntax #
  * Full S-Expressions. Everything is an atom (literal number, alphanum symbol, or paren'd list)
  * Function:
  "(fun ((: arg1 type1) ...) body)"
  where body is a single expression, the return value of the call
  functions act as closures
  * Let binding:
  "(let (((: name1 type1) exp1) ...) body)"
  where body is evaluated with names bound to exprs and value returned
  * Variable reference:
  "name" where name is in scope (introduced by let or function)
  * Function Application
  "(expr1 [expr2 ...])"
  expr1 evaluted to function, remaining expressions evaluated,
  bound to args of function expr1, and body is evaluated
  * An expression is a function definition, a let binding,
  a variable reference, a function application, or a literal number.
  * Early extension: automatic currying on partial application + hook into closure storage

# Semantics #
  * Function is evaluated, then arguments, then body of application
  * Let binding arms evaluated, then body
  * All bindings have lexical scope, shadowing more distant bindings
  * Execution runs through top level scope in order, returning (printing) the results

# Data layout #
  * All values are a 8 (4?) byte word
  * Integers are unboxed
  * Everything else is pointer to GC'd heap
  * Distinguish solely Integer vs ptr w/ tag bit (?)
  * Functions are a pointer to a N slot heap allocation, where slot 0 is the code ptr, and
  the remaining slots are the N-1 closure values, captured at definition time
  * Later extension: other/larger unboxed types
