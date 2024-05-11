(* Ocamllex scanner for TreeLang *)

{
open Treelangparse
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter (digit | letter | '_')*

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "->" { ARROW }
| '=' { EQUALS }
| '.' { DOT }
| "Root" { ROOT }
| "node" { NODE }
| "connect" { CONNECT }
| "forward_operation" { FORWARD_OPERATION }
| "backward_operation" { BACKWARD_OPERATION }
| "sum" { SUM }
| "percentage" { PERCENTAGE }
| "int" { INT_TYPE }
| ['A'-'Z']['a'-'z']* as node_type { NODE_TYPE(node_type) }
| digit+ as lem { INT_VALUE(int_of_string lem) }
| id as lem { ID(lem) }
| eof { EOF }