let get_next_expr () =
  let _ = Parse.Parse.empty_context in
  let e = Parse.Parse.parse_expression in
  match e Parse.Parse.empty_context with
  | Ok (v, _) ->
    let s = Ast.PrettyPrint.pp_expr v in
    print_string "AST ";
    print_string (Ast.pp_expr v);
    print_endline "";
    v
  | Error (msg, _ctx) -> raise @@ Failure ("Failed to parse expression: " ^ msg)
;;

let process ast_e =
  let ctx = Ir.empty_ctx () in
  let ir_e = Ir.from_ast_expr ctx ast_e in
  print_string "IR ";
  print_endline (Ir.PrettyPrint.pp_expr ir_e);
  print_endline "";
  let open Compile.Compiler in
  let c_action = compile ir_e in
  let ops = run_empty c_action in
  let strs =
    Dynarray.mapi (fun i op -> string_of_int i ^ " " ^ Bytecode.BC.pp_op op ^ "\n") ops
  in
  let s = Dynarray.fold_left ( ^ ) "Compiled\n" strs in
  print_string s
;;

let run1 () =
  print_endline "Debugging...";
  let e = get_next_expr () in
  process e
;;

let () = run1 ()
