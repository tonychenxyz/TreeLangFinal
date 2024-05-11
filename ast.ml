(* Abstract Syntax Tree and functions for printing it *)

type expr =
  | IntLit of int

type direction =
  | Forward
  | Backward

type node_type =
  | Root
  | Node

type data_type =
  | IntType

type operation =
  | Sum
  | Percentage

type decl =
  | NodeTypeDecl of node_type * string * data_type * data_type
  | ConnectDecl of string * string
  | NodeCreationDecl of string * string * string * expr option
  | OperationDecl of string * direction * operation

type program = decl list

(* Pretty-printing functions *)

let string_of_node_type = function
  | Root -> "Root"
  | Node -> "node"

let string_of_data_type = function
  | IntType -> "int"

let string_of_direction = function
  | Forward -> "forward"
  | Backward -> "backward"

let string_of_operation = function
  | Sum -> "sum"
  | Percentage -> "percentage"

let string_of_expr = function
  | IntLit(l) -> string_of_int l

let string_of_decl = function
  | NodeTypeDecl(node_type, name, forward_type, backward_type) ->
      string_of_node_type node_type ^ " " ^ name ^ " " ^
      string_of_data_type forward_type ^ " " ^
      string_of_data_type backward_type ^ "\n"
  | ConnectDecl(parent, child) ->
      "connect " ^ parent ^ " -> " ^ child ^ "\n"
  | NodeCreationDecl(parent, node_type, name, value_opt) ->
      parent ^ " -> " ^ node_type ^ " " ^ name ^
      (match value_opt with
       | Some value -> " = " ^ string_of_expr value
       | None -> "") ^ "\n"
  | OperationDecl(node_type, direction, operation) ->
      node_type ^ "." ^ string_of_direction direction ^ "_operation = " ^
      string_of_operation operation ^ "\n"

let string_of_program decls =
  "\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_decl decls) ^ "\n"