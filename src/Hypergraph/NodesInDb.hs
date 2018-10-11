-- | based on
-- https://www.reddit.com/r/haskell/comments/8tnily/dependent_types_in_constructors_eg_data_x_y_z_y/e191ppc/

-- | Something's wrong; see "foo" and "bar", commented out at the bottom.

{-# LANGUAGE
GADTs
, DataKinds
, ScopedTypeVariables
#-}

module Hypergraph.NodesInDb where

import Data.Hashable

import Hypergraph.Util
import Hypergraph.LengthedVector


type Label = String
type Arity = SNat
type ID = Int -- ^ Each Node has an ID
type NodeOrId = Either Node Int
  -- ^ like the WorkInfo type in WorksForTest.hs

-- | Relationships come in various flavors. For instance,
-- "some _ go _ sometimes" is a binary relationship, and like all binnary
-- relationships, it has a 3-part label.
-- PITFALL: Since Arity uses SNat and Vect uses Nat, the arity
-- is only one less than the vect length (not 2 as might appear below).
-- PITFALL: The least value of `n` possible is 'Z, which confusingly
-- corresponds to an arity of 1, not 0 (again because Arity = SNat).
type Flavor n = (Arity n, Vect ('S ('S n)) ID)

data Node where
  Atom :: Label -> Node
  RelationshipFlavor :: Flavor n -> Node
  Relationship :: forall n.
    Flavor n -> Vect ('S n) Node -> Node

instance Hashable Node where
  hashWithSalt x (Atom s) = x ## "Node Atom" ## s
  hashWithSalt x (RelationshipFlavor (a,b)) =
    x ## hash "Node RelationshipFlavor" ## a ## b
  hashWithSalt x (Relationship f v) =
    x ## hash "Node Relationship" ## f ## v

-- bar :: Node
-- bar = Relationship
--       (one, "The" >- "is cool." >- VNil)
--       (Atom "shark" >- VNil)
-- 
-- foo :: Node
-- foo = Relationship
--       (two, "The" >- "goes before the" >- "." >- VNil)
--       (Atom "horse" >- Atom "cart" >- VNil)

