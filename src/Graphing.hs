module Graphing (Node, Edge, Graph) where

-- A tuple struct which stores node information.
type Node = (Int, Int)

-- A tuple struct which describes a relation between two Nodes.
type Edge = (Node, Node)

-- A vector which stores relation information.
type Graph = [Edge]

