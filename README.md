# dot-edit

This repository contains the source code for `dot-edit`, a work-in-progress application for manipulating and generating simple graphs from DOT scripts.

## How Graphs are Stored

Graphs are stored using very basic DOT scripts. Storing additional metadata will probably not be implemented any time soon.

## Features

The `lib_core` directory includes the following modules:
 - `Graph`: includes functions for manipulating graphs and/or taking the symmetric, reflexive, and transitive closures of those graphs.
 - `Pathing`: includes functions for finding the depth-first, breadth-first, and best-first traversals of a graph.

The `lib_dot` directory includes the following modules:
 - `Dot`: includes functions for importing and/or exporting DOT scripts as valid graphs which can be used by `lib_core`.
 - `Parser`: monadic recursive descent parser.

