   open Ast

   type action = Ast | Sast | LLVM_IR
   
   let () =
     let action = ref LLVM_IR in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
     ] in
     let usage_msg = "usage: ./treelang.native [-a|-s|-l] [file.mc]" in
     let channel = ref stdin in
     Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
   
     let lexbuf = Lexing.from_channel !channel in
   
     try
       let ast = Treelangparse.program Scanner.token lexbuf in
       match !action with
       | Ast -> print_string (Ast.string_of_program ast)
       | _ ->
         let sast = Semant.check ast in
         match !action with
         | Ast     -> ()
         | Sast    -> print_string (Sast.string_of_sprogram sast)
         | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast))
     with
     | Parsing.Parse_error ->
       let pos = lexbuf.Lexing.lex_curr_p in
       Printf.eprintf "Syntax error at line %d, character %d\n"
         pos.Lexing.pos_lnum
         (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
       exit 1