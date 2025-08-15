let get_next_expr () =
  let _ = Parse.Parse.empty_context in
  let e = Parse.Parse.parse_expression in
  match e Parse.Parse.empty_context with
  | Ok (v, _) ->
    print_string "AST ";
    print_string (Ast.PrettyPrint.pp_expr v);
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
  let c_action = then_stop (compile ir_e) in
  let statics, ops = run_empty ~static_offset:0 c_action in
  let strs =
    Dynarray.mapi (fun i op -> string_of_int i ^ " " ^ Bytecode.BC.pp_op op ^ "\n") ops
  in
  let s = Dynarray.fold_left ( ^ ) "Compiled\n" strs in
  print_string s;
  let ss = Dynarray.length statics in
  let cs = Dynarray.length ops in
  let hs = 4092 in
  let remainder = 8192 - ss - cs - hs in
  print_endline @@ "Of 8192, " ^ string_of_int remainder ^ " left for stack";
  let open Interpret.Interpreter in
  init_mem
    ~mem_size:8192
    ~static_off:0 ~statics:statics
    ~stack_size:remainder ~stack_off:(ss + cs)
    ~heap_size:hs ~heap_off:(ss + cs + remainder)
    ~code_off:ss ~code:ops;
  print_endline "Running...";
  run_until_reached ~final:(ss+(Dynarray.length ops)-1);
  print_endline "Final Stack:";
  print_endline @@ pp_stack ()
;;

let run1 () =
  print_endline "Debugging...";
  let e = get_next_expr () in
  process e
;;

let () = run1 ()
