# A* Pathfinding

This repository contains the source code for `hay-star`; a work-in-progress application written in Haskell for graphing and path finding.

Graphs are stored as binary relations between nodes.

## Features

The `Graph` module includes functions for manipulating graphs and/or taking the symmetric, reflexive, and transitive closures of your graphs.

The `Pathing` module includes functions for finding the depth-first, breadth-first, best-first, *and* A* traversals of a graph. You can either use `traversal` to traverse the entire graph, or `path` to find the *first* path between two nodes (not necessarily the shortest path).
