{-# LANGUAGE
GADTs
, KindSignatures
, DataKinds
, ScopedTypeVariables
#-}

module Hypergraph.LengthedVector where

import Data.Hashable

import Hypergraph.Util


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
