open OUnit2
open Treelangparse
open Scanner
open Semant
open Ast
open Sast

(* Lexer tests *)
let lexer_test name input expected_output =
  name >:: (fun _ ->
    let lexbuf = Lexing.from_string input in
    let rec get_tokens acc =
      match token lexbuf with
      | EOF -> List.rev acc
      | r -> get_tokens (r :: acc)
    in
    assert_equal expected_output (get_tokens [])
  )

let lexer_tests = [
  lexer_test "test_root" "Root" [ROOT];
  lexer_test "test_arrow" "->" [ARROW];
  lexer_test "test_equals" "=" [EQUALS];
  lexer_test "test_node" "node" [NODE];
  lexer_test "test_int_type" "int" [INT_TYPE];
  lexer_test "test_id" "identifier123" [ID("identifier123")];
  lexer_test "test_int_value" "123" [INT_VALUE(123)];
  lexer_test "test_whitespace_handling" "  Root  " [ROOT];
  lexer_test "test_multiple_tokens" "Root node int" [ROOT; NODE; INT_TYPE];
  lexer_test "test_complex_identifiers" "node123 DirectoryNode int" [ID("node123"); ID("DirectoryNode"); INT_TYPE];
  lexer_test "test_symbols" "-> = ->" [ARROW; EQUALS; ARROW];
  lexer_test "test_large_numbers" "1234567890" [INT_VALUE(1234567890)];
  lexer_test "test_numeric_followed_by_keyword" "123Root" [INT_VALUE(123); ROOT];
  lexer_test "test_sequential_identifiers" "root node1 node2" [ID("root"); ID("node1"); ID("node2")];
  lexer_test "test_whitespace_and_tokens" "\n\t Root \n->\t int" [ROOT; ARROW; INT_TYPE];
]

(* Parser tests *)
let parser_test name input expected_output =
  name >:: (fun _ ->
    let lexbuf = Lexing.from_string input in
    let ast = Treelangparse.program Scanner.token lexbuf in
    assert_equal expected_output ast
  )

let parser_tests = [
  parser_test "test_node_type_decl" "Root root int int"
    [NodeTypeDecl(Root, "root", IntType, IntType)];
  (* parser_test "test_connect_decl" "connect root -> node"
    [ConnectDecl("root", "node")]; *)
  parser_test "test_node_creation_with_value"
    "root -> Node node1 = 100"
    [NodeCreationDecl("root", "Node", "node1", Some(IntLit(100)))];
  parser_test "test_node_declaration_directory" "node Directory int int"
    [NodeTypeDecl(Node, "Directory", IntType, IntType)];
  parser_test "test_node_declaration_file" "node File int int"
    [NodeTypeDecl(Node, "File", IntType, IntType)];
  parser_test "test_connection_root_directory" "root -> Directory dir1 = 200"
    [NodeCreationDecl("root", "Directory", "dir1", Some(IntLit(200)))];
  parser_test "test_multi_node_connection" "dir1 -> File file1 = 200"
    [NodeCreationDecl("dir1", "File", "file1", Some(IntLit(200)))];
  parser_test "test_deep_node_connection" "file1 -> File file2 = 200"
    [NodeCreationDecl("file1", "File", "file2", Some(IntLit(200)))];
  parser_test "test_multiple_node_creations" 
    "node A int int\nnode B int int\nroot -> A a1 = 100\na1 -> B b1 = 200"
    [NodeTypeDecl(Node, "A", IntType, IntType); NodeTypeDecl(Node, "B", IntType, IntType); 
     NodeCreationDecl("root", "A", "a1", Some(IntLit(100))); NodeCreationDecl("a1", "B", "b1", Some(IntLit(200)))];
  parser_test "test_nested_structures"
    "node X int int\nnode Y int int\nx -> X x1 = 50\nx1 -> Y y1 = 150"
    [NodeTypeDecl(Node, "X", IntType, IntType); NodeTypeDecl(Node, "Y", IntType, IntType);
     NodeCreationDecl("x", "X", "x1", Some(IntLit(50))); NodeCreationDecl("x1", "Y", "y1", Some(IntLit(150)))];
	parser_test "test_single_node_declaration"
    "node Simple int int"
    [NodeTypeDecl(Node, "Simple", IntType, IntType)];
  parser_test "test_root_connection"
    "Root root int int\nroot -> Simple simple = 50"
    [NodeTypeDecl(Root, "root", IntType, IntType); NodeCreationDecl("root", "Simple", "simple", Some(IntLit(50)))];
  parser_test "test_empty_node_creation"
    "node Empty int int\nroot -> Empty empty"
    [NodeTypeDecl(Node, "Empty", IntType, IntType); NodeCreationDecl("root", "Empty", "empty", None)];
]

(* Semantic analysis tests *)
let semant_test name input expected_output =
  name >:: (fun _ ->
    let lexbuf = Lexing.from_string input in
    let ast = Treelangparse.program Scanner.token lexbuf in
    let result = 
      try Some (check ast) with
      | Failure msg -> None
    in
    assert_equal expected_output result
  )

let semant_tests = [
  semant_test "test_duplicate_node" 
    "Root root int int\nRoot root int int"
    None;  (* Expected to fail due to duplicate node declarations *)
  semant_test "test_undefined_node" 
    "root -> Node node1 = 10"
    None;  (* Expected to fail because root node is not defined *)
  semant_test "test_invalid_node_reference"
    "Root root int int\nroot -> Nonexistent node1 = 10"
    None;  (* Expected to fail because Nonexistent is not a defined node type *)
  (*semant_test "test_valid_root_declaration"
    "Root root int int"
    (Some {
      nodes = [{node_type = SRoot; name = "root"; forward_type = SIntType; backward_type = SIntType;
                forward_value = None; backward_value = None}];
      decls = [];
    }); *)
]

(* LLVM IR generation tests *)
(* Commented out to avoid LLVM setup issues *)
(*
let irgen_test name sast expected_ir =
  name >:: (fun _ ->
    let module_string = Llvm.string_of_llmodule (translate sast) in
    assert_equal expected_ir module_string ~printer:(fun x -> x)
  )

let sample_sast = {
  nodes = [{node_type = SRoot; name = "root"; forward_type = SIntType; backward_type = SIntType; 
            forward_value = None; backward_value = None}];
  decls = []
}

let sample_ir = "; ModuleID = 'TreeStats'\n" ^
                "source_filename = \"TreeStats\"\n" ^
                "...\n" (* Simulate expected IR structure *)
                "define i32 @main() {\n" ^
                "entry:\n" ^
                "  ret i32 0\n" ^
                "}\n"

let irgen_tests = [
  irgen_test "test_basic_ir_generation" sample_sast sample_ir;
  (* Add more LLVM IR generation tests as needed *)
]
*)

(* Run all tests *)
let () =
  let suite =
    "TreeLang Tests" >::: [
      "Lexer Tests" >::: lexer_tests;
      "Parser Tests" >::: parser_tests;
      "Semantic Tests" >::: semant_tests;
      (* "IRGen Tests" >::: irgen_tests; *)
    ]
  in
  run_test_tt_main suite
