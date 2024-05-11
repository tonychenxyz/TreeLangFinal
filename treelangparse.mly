/* Ocamlyacc parser for TreeLang */

%{
open Ast
%}

%token ROOT NODE CONNECT ARROW EQUALS DOT
%token FORWARD_OPERATION BACKWARD_OPERATION
%token SUM PERCENTAGE INT_TYPE
%token <string> ID
%token <int> INT_VALUE
%token <string> NODE_TYPE
%token EOF

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { [] }
  | decl decls { $1 :: $2 }

decl:
  | node_type_decl { $1 }
  | connect_decl { $1 }
  | node_creation_decl { $1 }
  | operation_decl { $1 }

node_type_decl:
  | ROOT ID INT_TYPE INT_TYPE { NodeTypeDecl(Root, $2, IntType, IntType) }
  | NODE NODE_TYPE INT_TYPE INT_TYPE { NodeTypeDecl(Node, $2, IntType, IntType) }

connect_decl:
  | CONNECT NODE_TYPE ARROW NODE_TYPE { ConnectDecl($2, $4) }

node_creation_decl:
  | ID ARROW NODE_TYPE ID { NodeCreationDecl($1, $3, $4, None) }
  | ID ARROW NODE_TYPE ID EQUALS INT_VALUE { NodeCreationDecl($1, $3, $4, Some (IntLit $6)) }

operation_decl:
  | NODE_TYPE DOT FORWARD_OPERATION EQUALS operation { OperationDecl($1, Forward, $5) }
  | NODE_TYPE DOT BACKWARD_OPERATION EQUALS operation { OperationDecl($1, Backward, $5) }

operation:
  | SUM { Sum }
  | PERCENTAGE { Percentage }