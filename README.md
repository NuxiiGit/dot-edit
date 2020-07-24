# dot-edit

This repository contains the source code for `dot-edit`, a simple command-line application for manipulating and generating simple graphs from DOT scripts.

## Examples

Taking the transitive closure of a graph
```
~$ dot-edit 'digraph { a -> b; b -> c; }' transitive
digraph {
  a -> b;
  b -> c;
  a -> c;
}
```

Creating custom graphs without a template
```
~$ dot-edit add-edge:a:b reflexive
digraph {
  a -> b;
  a -> a;
  b -> b;
}
```

## Features

This application only supports a limited subset of the DOT language grammar. It should be used for very basic relation-like graphs and trees. The currently supported features are
 - methods of adding and removing edges or vertices from a graph
 - methods of taking the symmetric, reflexive, transitive, and antisymmetric closures of graphs
 - method of composing two graphs
 - methods of transforming a graph into an equivalence or order relation
 - methods of performing boolean operations on graphs: taking the union, disjoint union, intersection, and set difference of two graphs
 - methods of computing the depth-first, breadth-first, and best-first traversals of a graph or tree
 - ability to parse and encode simple DOT scripts
 - supports directed (`digraph { .. }`) and undirected (`graph { .. }`) graphs
 - supports paths: `digraph { a -> b -> c -> d; }` (equivalent to `digraph { a -> b; b -> c; c -> d; }`)
 - supports clusters: `digraph { root -> { x y z }; }` (equivalent to `digraph { root -> x; root -> y; root -> z; }`)
 - supports alphanumeric (`abc`), numeral (`-12.3`), and literal (`"hello world"`) identifiers
 - supports automatic semi-colon and comma insertion
 - supports line and multi-line comments
 - supports pretty printing of DOT graphs
