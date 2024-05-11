(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR *)

   module L = Llvm
   module A = Ast
   open Sast
   
   module StringMap = Map.Make(String)
   
   (* translate : sprogram -> Llvm.module *)
   let translate (sprogram : sprogram) =
     let context = L.global_context () in
   
     (* Create the LLVM compilation module *)
     let the_module = L.create_module context "TreeStats" in
   
     (* Get types from the context *)
     let i32_t = L.i32_type context in
   
     (* Declare printf function *)
     let printf_t = L.var_arg_function_type i32_t [| L.pointer_type (L.i8_type context) |] in
     let printf_func = L.declare_function "printf" printf_t the_module in
   
     (* Create a map of nodes *)
     let node_map = 
      List.fold_left (fun m decl ->
        match decl with 
        | SNodeCreationDecl (parent, node_type, name, value_opt) ->
          let node = {
            node_type = (match node_type with 
              | "Root" -> SRoot
              | _ -> SNode);
            name = name;
            forward_type = SIntType; (* Assuming default types for now *)
            backward_type = SIntType;
            forward_value = value_opt;
            backward_value = None;
          } in
          StringMap.add name node m
        | _ -> m
      ) StringMap.empty sprogram.decls
    in
     (* let print_node_map_keys node_map =
       StringMap.iter (fun key _ -> Printf.printf "%s\n" key) node_map
     in
     print_node_map_keys node_map; *)
   
     (* Count the total number of nodes *)
     let num_nodes = List.fold_left (fun count decl ->
      match decl with
      | SNodeCreationDecl (_, _, _, _) -> count + 1
      | _ -> count
    ) 0 sprogram.decls in
    
     (* Calculate the maximum depth of the tree *)
     let max_depth =
      let rec max_depth' node depth =
        (* Printf.printf "Node: %s, Depth: %d\n" node.name depth; *)
        let child_depths = List.fold_left (fun acc decl ->
          match decl with
          | SNodeCreationDecl (parent, _, child, _) ->
            (* Printf.printf "Parent: %s, Child: %s\n" parent child; *)
            if parent = node.name then
              begin
                (* Printf.printf "Processing child: %s\n" child; *)
                try
                  let child_node = StringMap.find child node_map in
                  let child_depth = max_depth' child_node (depth + 1) in
                  (* Printf.printf "Child depth: %d\n" child_depth; *)
                  max child_depth acc
                with Not_found ->
                  (* Printf.printf "Child node not found: %s\n" child; *)
                  acc
              end
            else
              begin
                (* Printf.printf "Skipping child: %s\n" child; *)
                acc
              end
          | _ ->
            (* Printf.printf "Skipping non-node declaration\n"; *)
            acc
        ) depth sprogram.decls in
        (* Printf.printf "Max depth of children: %d\n" child_depths; *)
        child_depths
      in
      try
        let root_node = {
          node_type = SRoot;
          name = "root";
          forward_type = SIntType;
          backward_type = SIntType;
          forward_value = None;
          backward_value = None;
        } in
        let max_depth_value = max_depth' root_node 0 in
        (* Printf.printf "Max depth: %d\n" max_depth_value; *)
        max_depth_value
      with Not_found ->
        (* Printf.printf "Root node not found\n"; *)
        0
      in
    
     (* Build the main function *)
     let main_ty = L.function_type i32_t [||] in
     let main = L.define_function "main" main_ty the_module in
     let builder = L.builder_at_end context (L.entry_block main) in
   
     (* Print the number of nodes *)
     let num_nodes_str = L.build_global_stringptr ("Number of nodes: %d\n") "num_nodes_str" builder in
     let num_nodes_printf = L.build_call printf_func [| num_nodes_str; L.const_int i32_t num_nodes |] "num_nodes_printf" builder in
     ignore(num_nodes_printf);
   
     (* Print the maximum depth *)
     let max_depth_str = L.build_global_stringptr ("Max depth: %d\n") "max_depth_str" builder in
     let max_depth_printf = L.build_call printf_func [| max_depth_str; L.const_int i32_t max_depth |] "max_depth_printf" builder in
     ignore(max_depth_printf);
    
     (* Return 0 *)
     ignore(L.build_ret (L.const_int i32_t 0) builder);
     the_module