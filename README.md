# A* Pathfinding

This repository contains the source code for `hay-star`; a work-in-progress application written in Haskell for graphing and path finding.

Graphs are stored as binary relations between nodes.

## Features

The `lib_core` directory includes the following modules:
 - `Graph`: includes functions for manipulating graphs and/or taking the symmetric, reflexive, and transitive closures of those graphs.
 - `Tree`: converts graphs into infinite trees.
 - `Pathing`: includes functions for finding the depth-first, breadth-first, best-first, *and* A* traversals of a graph.

The `lib_dot` directory includes the following modules:
 - `Dot`: includes functions for importing and/or exporting DOT scripts as valid graphs which can be used by `lib_core`.
 - `Parser`: monadic recursive descent parser.

