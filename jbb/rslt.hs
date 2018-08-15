-- | based on
-- https://www.reddit.com/r/haskell/comments/8tnily/dependent_types_in_constructors_eg_data_x_y_z_y/e191ppc/

{-# LANGUAGE GADTs, KindSignatures, DataKinds, ScopedTypeVariables #-}

data Nat = Z | S Nat

data SNat n where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

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

infixr 5 >-
(>-) = VCons

instance Show a => Show (Vect n a) where
  show vec = "<<" ++ go vec ++ ">" where
    go :: forall m a. Show a => Vect m a -> String
    go VNil = ""
    go (a `VCons` vec) = " " ++ show a ++ " >" ++ go vec

type Label = String
type Arity = SNat

type Flavor n = (Arity n, Vect ('S ('S n)) String)

ar = (one, "a" >- "b" >- VNil) :: Flavor 'Z

data HypergraphNode where
  Atom :: Label -> HypergraphNode
  RelationshipFlavor :: Flavor n -> HypergraphNode
  Relationship :: Flavor n -> Vect ('S n) HypergraphNode -> HypergraphNode

foo :: HypergraphNode
foo = Relationship
      (two, "The" >- "comes before the" >- "" >- VNil)
      (Atom "a" >- Atom "b" >- VNil)
