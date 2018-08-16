module Hypergraph.Util where

import Data.Hashable


infixl 5 ##
(##) :: Hashable a => Int -> a -> Int
(##) = hashWithSalt
