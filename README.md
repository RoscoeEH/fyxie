# Tentative structure #

# Passes / Phases: #
  * tokenizer/lexer: Character stream -> token stream
  * parser: token stream -> Concrete syntax tree
  * Cst -> Ast
  * Ast -> IR / bytecode
  * Ir/bytecode assembler (or interpreter)

# Types #
  * only primitive type is Int
  * Type names start with a capital letter
  * Arity checking on function application
  * Early extension: More than int and function types
  * Early extension: Datatypes
  (finite constructors, each with a 0+ input types)
  (can define unit, etc with these)
  * Early extension: Type aliases
  (maps immediately to the target type, shortens name etc)
  * Later extension: Type arguments

# Syntax #
  * Full S-Expressions. Everything is an atom (literal number, alphanum symbol, or paren'd list)
  * Type:
  primitive type name or "((type ...) -> type)"
  * Function:
  "(fun ((: arg1 type1) ...) body)"
  where body is a single expression, the return value of the call.
  Functions act as closures
  * Let binding:
  "(let (((: name1 type1) exp1) ...) body)"
  Where body is evaluated with names bound to exprs and value returned. No ordering
  is enforced on binding arms, and the bound names aren't in scope until the body.
  * Variable reference:
  "name" where name is in scope (introduced by let or function)
  * Function Application
  "(expr1 [expr2 ...])"
  expr1 evaluted to function, remaining expressions evaluated,
  bound to args of function expr1, and body is evaluated. No ordering is enforced on the evaluation of expressions involved.
  * An expression is a function definition, a let binding,
  a variable reference, a function application, or a literal number.
  * Early extension: automatic currying on partial application + hook into closure storage

# Semantics #
  * All bindings have lexical scope, shadowing more distant bindings
  * Execution runs through top level scope in order, returning (printing) the results

# Data layout #
  * All values are a 8 (4?) byte word
  * Integers are unboxed
  * Everything else is pointer to GC'd heap
  * Distinguish solely Integer vs ptr w/ tag bit (?)
  * Functions are a pointer to a N slot heap allocation, where slot 0 is size
  of the remainder of the closure followed by the closure values, followed by the code ptr.
  * Later extension: other/larger unboxed types
  (GC alignment gives us some number of low order bits to tag ptrs / values with)

