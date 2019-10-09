module Graph (Graph, Node) where

import Relation

-- |A type alias which describes node information.
type Node = (Int, Int)

-- |A type alias which describes graph information.
type Graph = Relation Node