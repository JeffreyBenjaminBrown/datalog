-- | based on
-- https://www.reddit.com/r/haskell/comments/8tnily/dependent_types_in_constructors_eg_data_x_y_z_y/e191ppc/

{-# LANGUAGE GADTs, KindSignatures, DataKinds, ScopedTypeVariables #-}

module Jbb.Rslt where

import Data.Hashable


infixl 5 ##
(##) :: Hashable a => Int -> a -> Int
(##) = hashWithSalt

data Nat = Z | S Nat

instance Hashable Nat where
  hashWithSalt x nat = x ## hash "Nat" ## go nat where
    go :: Nat -> Integer
    go Z = 0
    go (S x) = 1 + go x

data SNat n where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

instance Hashable (SNat n) where
  hashWithSalt x snat = x ## "SNat" ## go snat
    where go :: forall n. SNat n -> Int
          go SZ = 0
          go (SS x) = 1 + go x

instance Show (SNat n) where
  show s = "type-level " ++ show (go s) where
    go :: forall n. SNat n -> Int
    go SZ = 1
    go (SS k) = 1 + go k

one :: SNat 'Z
one = SZ
two :: SNat ('S 'Z)
two = SS SZ
three :: SNat ('S ('S 'Z))
three = SS $ SS SZ

data Vect (n :: Nat) a where
  VNil  :: Vect 'Z a
  VCons :: a -> Vect n a -> Vect ('S n) a

instance Hashable a => Hashable (Vect n a) where
  hashWithSalt x (VNil) = x ## "Vect" ## "Nil"
  hashWithSalt x (a `VCons` b) = x ## a ## "Vect" ## b

infixr 5 >-
(>-) = VCons

instance Show a => Show (Vect n a) where
  show vec = "<<" ++ go vec ++ ">" where
    go :: forall m a. Show a => Vect m a -> String
    go VNil = ""
    go (a `VCons` vec) = " " ++ show a ++ " >" ++ go vec

type Label = String
type Arity = SNat

-- | Relationships come in various flavors. For instance,
-- "some _ go _ sometimes" is a binary relationship, and like all binnary
-- relationships, it has a 3-part label.
-- PITFALL: Since Arity uses SNat and Vect uses Nat, the arity
-- is only one less than the vect length (not 2 as might appear below).
-- PITFALL: The least value of `n` possible is 'Z, which confusingly
-- corresponds to an arity of 1, not 0 (again because Arity = SNat).
type Flavor n = (Arity n, Vect ('S ('S n)) String)

data HypergraphNode where
  Atom :: Label -> HypergraphNode
  RelationshipFlavor :: Flavor n -> HypergraphNode
  Relationship :: forall n.
    Flavor n -> Vect ('S n) HypergraphNode -> HypergraphNode

instance Hashable HypergraphNode where
  hashWithSalt x (Atom s) = x ## "HypergraphNode Atom" ## s
  hashWithSalt x (RelationshipFlavor (a,b)) =
    x ## hash "HypergraphNode RelationshipFlavor" ## a ## b
  hashWithSalt x (Relationship f v) =
    x ## hash "HypergraphNode Relationship" ## f ## v

bar :: HypergraphNode
bar = Relationship
      (one, "The" >- "is cool." >- VNil)
      (Atom "shark" >- VNil)

foo :: HypergraphNode
foo = Relationship
      (two, "The" >- "goes before the" >- "." >- VNil)
      (Atom "horse" >- Atom "cart" >- VNil)
