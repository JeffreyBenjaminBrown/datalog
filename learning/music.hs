import Data.Hashable
import Data.Set ( fromList )
import Data.Text ( Text )
import Database.Datalog


type Sound = String
type Dur = Int

data Depic = DSound Dur Sound
           | Seq Depic Depic
           | Simult Depic Depic
  deriving (Eq, Ord, Show)

(##) = hashWithSalt

instance Hashable Depic where
  hashWithSalt x (DSound d s) = x ##      d ## hash s ## (1 :: Int)
  hashWithSalt x (Seq d e)    = x ## hash d ## hash e ## (2 :: Int)
  hashWithSalt x (Simult d e) = x ## hash d ## hash e ## (3 :: Int)
