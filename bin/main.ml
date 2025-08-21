let init_and_run statics ops =
  let ss = Dynarray.length statics in
  let cs = Dynarray.length ops in
  let hs = 4092 in
  let remainder = 8192 - ss - cs - hs in
  print_endline @@ string_of_int ss ^ " of statics, " ^ string_of_int cs ^ " of ops";
  print_endline @@ "Of 8192, " ^ string_of_int remainder ^ " left for stack";
  let statics_strs =
    Dynarray.mapi (fun i slot -> "\t" ^ string_of_int i ^ "\t" ^ Bytecode.BC.pp_slot slot ^ "\n") statics
  in
  print_string @@ Dynarray.fold_left ( ^ ) "\nStatics\n" statics_strs;
  let ops_strs =
    Dynarray.mapi (fun i op -> "\t" ^ string_of_int (i+ss) ^ "\t" ^ Bytecode.BC.pp_op op ^ "\n") ops
  in
  print_string @@ Dynarray.fold_left ( ^ ) "\nCompiled\n" ops_strs;
  let open Interpret.Interpreter in
  init_mem
    ~mem_size:8192
    ~static_off:0 ~statics:statics
    ~stack_size:remainder ~stack_off:(ss + cs)
    ~heap_size:hs ~heap_off:(ss + cs + remainder)
    ~code_off:ss ~code:ops;
  print_endline "\nRunning...";
  run_until_reached ~final:(ss+(Dynarray.length ops)-1);
  print_endline "Final Stack:";
  print_endline @@ pp_stack ()
;;

let handle_module () =
  (* TODO switch this to be per file or something, work by changing the channel in Lex *)
  let open Parse.Parse in
  let open Ast in
  let ast_m,_ = match parse_mod empty_context with
    | Ok m -> m
    | Error (e,_) -> raise @@ Failure e
  in
  print_endline "Ast:";
  print_endline @@ PrettyPrint.pp_mod ast_m;
  
  let open Ir in
  let ctx = empty_ctx () in
  let () = declare_ahead ctx ast_m in
  let ir_m = from_ast_mod ctx ast_m in
  print_endline "\nIR:";
  print_endline @@ PrettyPrint.pp_mod ir_m;
  let open Compile.Compiler in
  let c_action = then_stop (Compile.Tracker.bind (static_pass_mod ir_m) (fun _ -> compile_mod ir_m)) in
  let statics, ops = run_empty ~static_offset:0 c_action in
  init_and_run statics ops
;;

let () =
  let () = 
    if Array.length Sys.argv <> 2
    then Parse.Lex.channel := In_channel.stdin
    else
      let f = Sys.argv.(1) in
      Parse.Lex.channel := In_channel.open_text f
  in
  handle_module ()

(*
 * let get_next_expr () =
 *   let _ = Parse.Parse.empty_context in
 *   let e = Parse.Parse.parse_expression in
 *   match e Parse.Parse.empty_context with
 *   | Ok (v, _) ->
 *     print_string "AST ";
 *     print_string (Ast.PrettyPrint.pp_expr v);
 *     print_endline "";
 *     v
 *   | Error (msg, _ctx) -> raise @@ Failure ("Failed to parse expression: " ^ msg)
 * ;;
 * 
 * let process ast_e =
 *   let ctx = Ir.empty_ctx () in
 *   let ir_e = Ir.from_ast_expr ctx ast_e in
 *   print_string "IR ";
 *   print_endline (Ir.PrettyPrint.pp_expr ir_e);
 *   print_endline "";
 *   let open Compile.Compiler in
 *   let c_action = then_stop (compile ir_e) in
 *   let statics, ops = run_empty ~static_offset:0 c_action in
 *   init_and_run statics ops
 * ;;
 * 
 * let run1 () =
 *   print_endline "Debugging...";
 *   let e = get_next_expr () in
 *   process e
 * ;;
 *)
(*
 * let () = run1 ()
 *)
