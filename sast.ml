(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type direction =
  | SForward
  | SBackward

type node_type =
  | SRoot
  | SNode

type typ =
  | SIntType

type sexpr = typ * sx
and sx =
  | SIntLit of int

type snode = {
  mutable node_type: node_type;
  mutable name: string;
  mutable forward_type: typ;
  mutable backward_type: typ;
  mutable forward_value: sexpr option;
  mutable backward_value: sexpr option;
}

type sdecl =
  | SNodeTypeDecl of node_type * string * typ * typ
  | SConnectDecl of string * string
  | SNodeCreationDecl of string * string * string * sexpr option
  | SOperationDecl of string * direction * operation

and operation =
  | SSum
  | SPercentage

type sprogram = {
  mutable nodes: snode list;
  mutable decls: sdecl list;
}

(* Pretty-printing functions *)

let string_of_direction = function
  | SForward -> "forward"
  | SBackward -> "backward"

let string_of_node_type = function
  | SRoot -> "Root"
  | SNode -> "node"

let string_of_typ = function
  | SIntType -> "int"

let string_of_sexpr (t, sx) =
  "(" ^ string_of_typ t ^ " : " ^ (match sx with
    | SIntLit(l) -> string_of_int l
  ) ^ ")"

let string_of_operation = function
  | SSum -> "sum"
  | SPercentage -> "percentage"

let string_of_sdecl = function
  | SNodeTypeDecl(node_type, name, forward_type, backward_type) ->
      string_of_node_type node_type ^ " " ^ name ^ " " ^
      string_of_typ forward_type ^ " " ^
      string_of_typ backward_type ^ "\n"
  | SConnectDecl(parent, child) ->
      "connect " ^ parent ^ " -> " ^ child ^ "\n"
  | SNodeCreationDecl(parent, node_type, name, value_opt) ->
      parent ^ " -> " ^ node_type ^ " " ^ name ^
      (match value_opt with
       | Some value -> " = " ^ string_of_sexpr value
       | None -> "") ^ "\n"
  | SOperationDecl(node_type, direction, operation) ->
      node_type ^ "." ^ string_of_direction direction ^ "_operation = " ^
      string_of_operation operation ^ "\n"

let string_of_snode node =
  string_of_node_type node.node_type ^ " " ^ node.name ^ " {\n" ^
  "  forward_type: " ^ string_of_typ node.forward_type ^ "\n" ^
  "  backward_type: " ^ string_of_typ node.backward_type ^ "\n" ^
  (match node.forward_value with
   | Some value -> "  forward_value: " ^ string_of_sexpr value ^ "\n"
   | None -> "") ^
  (match node.backward_value with
   | Some value -> "  backward_value: " ^ string_of_sexpr value ^ "\n"
   | None -> "") ^
  "}\n"

let string_of_sprogram sprogram =
  "\n\nSemantically checked program: \n\n" ^
  "Nodes:\n" ^
  String.concat "" (List.map string_of_snode sprogram.nodes) ^ "\n" ^
  "Declarations:\n" ^
  String.concat "" (List.map string_of_sdecl sprogram.decls) ^ "\n"


  