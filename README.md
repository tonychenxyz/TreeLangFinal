
# Build instruction
```
ocamlbuild -r -pkg llvm treelang.native
./treelang.native -l example.tl > example.out
lli example.out
```

# Test Instruction
```
ocamlbuild -use-ocamlfind -pkg ounit2 test.native
./test.native 
```


# Language Description

1. The language consists of nodes and connections between them, representing a hierarchical structure.

2. You can declare node types using the "node" keyword followed by the type name.

3. Each node has a name and can optionally have an associated value using the "=" operator.

4. There must be a unique root node type declared, serving as the starting point of the structure.

5. Nodes are connected using the "->" operator, specifying the source node name and the target node name, followed by the associated value (if any).

6. The script should output the following statistics of the tree:
   - Number of nodes: The total count of nodes in the tree.
   - Max depth: The maximum depth of the tree, representing the longest path from the root to a leaf node.

Example:

```
Root root int int
node Category int int
node Item int int

root -> Category Electronics = 2000
root -> Category Clothing = 1500
Electronics -> Category Computers = 1200
Electronics -> Category Smartphones = 1000
Computers -> Item Laptop = 1200
Computers -> Item Desktop = 800
Smartphones -> Item iPhone = 999
Smartphones -> Item AndroidPhone = 799
Clothing -> Category Mens = 800
Clothing -> Category Womens = 700
Mens -> Item Shirt = 50
Mens -> Item Pants = 80
Womens -> Item Dress = 120
Womens -> Item Skirt = 60
```

Output:

```
Number of nodes: 15
Max depth: 4
```

In this language description:

- The root node is declared as `Root root int int`.
- Other node types are declared using the `node` keyword followed by the type name and `int int` (`Category int int` and `Item int int`).
- Connections between nodes are established using the `->` operator, specifying the source node name, followed by the target node type, target node name, and associated value (if any).
- Each connection is specified on a separate line.

The script will output the statistics of the tree, including the total number of nodes (15) and the maximum depth of the tree (4).