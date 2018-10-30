import Database.Datalog
import Data.Hashable


type ID = Int
data Atom = Atom !ID !String
data Lab1 = Lab1 !ID !ID
data Rel1 = Rel1 !ID !ID
data Lab2 = Lab2 !ID !ID !ID
data Rel2 = Rel2 !ID !ID !ID
data Lab3 = Lab3 !ID !ID !ID !ID
data Rel3 = Rel3 !ID !ID !ID !ID
