

let () = print_endline "test top"

(* TODO
 *
 * fix this to not use run_empty, since we should be threading the
 * monad through and/or change it to not be a delayed computation.
 *
 *)
let get_next_expr () =
  let _ = Parse.Parse.Parser.empty_context in
  let e = Parse.Parse.parse_expression in
  match e Parse.Parse.Parser.empty_context with
  | Ok (v, _) ->
    let s = Cst.pp_expr v in
    print_string "CST ";
    print_endline s;
    v
  | Error _ -> raise @@ Failure "Failed to parse expression"

let _ = get_next_expr ()

let _ = print_endline "ended"
(*
 * let process cst_e =
 *   let (_, ast_e) = Ast.from_cst [] [] cst_e in
 *   print_string "AST ";
 *   print_endline (Ast.pp_expr ast_e);
 *   let open Compile.Compiler in
 *   let c_action = compile ast_e in
 *   let ops = run_empty c_action in
 *   let s = Dynarray.fold_left (fun acc op -> acc ^ Bytecode.BC.pretty_print_op op ^ "\n") "Compiled\n" ops in
 *   print_string s
 * 
 * let run1 () =
 *   print_endline "Debugging...";
 *   let e = get_next_expr () in
 *   process e
 * 
 * let () = run1 ()
 *)
