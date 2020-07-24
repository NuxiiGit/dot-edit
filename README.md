# dot-edit

This repository contains the source code for `dot-edit`, a simple command-line application for manipulating and generating simple graphs from DOT scripts.

## Examples

Taking the transitive closure of a graph
```
~$ dot-edit 'digraph { a -> b; b -> c; }' transitive
digraph { a -> b; b -> c; a -> c; }
```

Creating custom graphs without a template
```
~$ dot-edit add-edge:a:b reflexive
digraph { a -> b; a -> a; b -> b; }
```

## Features

The `lib_core` directory includes the following modules:
 - `Graph`: includes functions for manipulating graphs and/or taking the symmetric, reflexive, and transitive closures of those graphs.
 - `Pathing`: includes functions for finding the depth-first, breadth-first, and best-first traversals of a graph.

The `lib_dot` directory includes the following modules:
 - `Dot`: includes functions for importing and/or exporting DOT scripts as valid graphs which can be used by `lib_core`.
 - `Parser`: monadic recursive descent parser.

