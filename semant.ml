(* Semantic checking for the TreeLang compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong. *)

let check (program) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : string list) =
    let rec dups = function
        [] -> ()
      | name1 :: name2 :: _ when name1 = name2 ->
        raise (Failure ("Error: duplicate\n" ^ kind ^ "\n" ^ name1 ^ "\n" ^ name2 ^ "\n" ^ String.concat "," binds))
      | _ :: t -> dups t
    in dups (List.sort compare binds)
  in

  (* Collect node type declarations into a symbol table *)
  let node_type_decls =
    let add_node_type map node_type_decl = match node_type_decl with
    NodeTypeDecl(_, name, _, _) ->
      StringMap.add (String.lowercase_ascii name) name map
  | _ -> map
    in
    let initial_map = StringMap.empty in  (* Start with an empty map *)
    List.fold_left add_node_type initial_map program
  in
  
  (* Debugging code to print out the contents of node_type_decls *)
  (* let () =
    StringMap.iter (fun key value -> Printf.printf "%s -> %s\n" key value) node_type_decls
  in *)

  (* Return a node type from our symbol table *)
  let find_node_type name =
    let lname = String.lowercase_ascii name in
    if lname = "root" then "Root"  (* Treat "root" as a special case *)
    else
      try StringMap.find lname node_type_decls
      with Not_found -> raise (Failure ("unrecognized node type " ^ name))
  in

  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr = function
      IntLit l -> (SIntType, SIntLit l)
  in

  let rec check_operation = function
      Sum -> SSum
    | Percentage -> SPercentage
  in

  let rec check_decl decl =
    match decl with
    | NodeTypeDecl(node_type, name, forward_type, backward_type) ->
      let snode_type = match node_type with
        | Root -> SRoot
        | Node -> SNode
      in
      let sforward_type = match forward_type with
        | IntType -> SIntType
      in
      let sbackward_type = match backward_type with
        | IntType -> SIntType
      in
      SNodeTypeDecl(snode_type, name, sforward_type, sbackward_type)
    | ConnectDecl(parent, child) ->
        let _ = find_node_type parent in
        let _ = find_node_type child in
        SConnectDecl(parent, child)
    | NodeCreationDecl(parent, node_type, name, value_opt) ->
        let _ = find_node_type node_type in 
        let value_sexpr_opt = match value_opt with
            None -> None
          | Some value_expr -> Some (check_expr value_expr)
        in
        SNodeCreationDecl(parent, node_type, name, value_sexpr_opt)
    | OperationDecl(node_type, direction, operation) ->
        let _ = find_node_type node_type in
        let sdirection = match direction with
            Forward -> SForward
          | Backward -> SBackward
        in
        let soperation = check_operation operation in
        SOperationDecl(node_type, sdirection, soperation)
  in

  let check_program program =
    let node_type_names = List.map (function NodeTypeDecl(_, name, _, _) -> name | _ -> "") program
                          |> List.filter (fun name -> name <> "") in
    let _ = check_binds "node type" node_type_names in
    let checked_nodes = List.filter_map (function
      | NodeTypeDecl(node_type, name, forward_type, backward_type) ->
        let snode_type = match node_type with Root -> SRoot | Node -> SNode in
        let sforward_type = match forward_type with IntType -> SIntType in
        let sbackward_type = match backward_type with IntType -> SIntType in
        Some { node_type = snode_type; name = name; forward_type = sforward_type; backward_type = sbackward_type;
               forward_value = None; backward_value = None }
      | _ -> None
    ) program in
    let checked_decls = List.map check_decl program in
    { nodes = checked_nodes; decls = checked_decls }
  in

  check_program program