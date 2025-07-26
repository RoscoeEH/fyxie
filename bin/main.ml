

let get_next_expr () =
  let _ = Parse.Parse.Parser.empty_context in
  let e = Parse.Parse.parse_expression in
  match e Parse.Parse.Parser.empty_context with
  | Ok (v, _) ->
    let s = Cst.pp_expr v in
    print_string "CST ";
    print_endline s;
    print_endline "";
    v
  | Error _ -> raise @@ Failure "Failed to parse expression"
;;

let process cst_e =
  let (_, ast_e) = Ast.from_cst [] [] cst_e in
  print_string "AST ";
  print_endline (Ast.pp_expr ast_e);
  print_endline "";
  let open Compile.Compiler in
  let c_action = compile ast_e in
  let ops = run_empty c_action in
  let strs = Dynarray.mapi (fun i op -> string_of_int i ^ " " ^ Bytecode.BC.pp_op op ^ "\n") ops in
  let s = Dynarray.fold_left (^) "Compiled\n" strs in
  print_string s

let run1 () =
  print_endline "Debugging...";
  let e = get_next_expr () in
  process e

let () = run1 ()
