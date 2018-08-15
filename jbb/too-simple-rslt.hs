{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

import Data.Hashable
import Database.Datalog

type Arity = Int

-- https://www.reddit.com/r/haskell/comments/8tnily/dependent_types_in_constructors_eg_data_x_y_z_y/e18upbc/
data ExprKind = WordK | TpltK | RelK
data Expr (a :: ExprKind) where
  Word :: String -> Expr 'WordK
  Tplt :: Arity -> [Expr 'WordK] -> Expr 'TpltK
  Rel :: Expr 'TpltK -> [Expr a] -> Expr 'WordK
