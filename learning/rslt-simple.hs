import Database.Datalog
import Data.Hashable


-- | Indices
--
-- An ID could refer to an atom, a relationship or a label,
-- but a RelID should refer only to a Rel, and a LabelID to a label.
-- (PITFALL: That's up to the coder, not enforced by the compiler.)
type ID = Int
type LabID = Int -- ^ the label on a relationship, 
type RelID = Int

-- | = Data types.
-- In each type, its own ID is listed first.
--
-- The label for an n-ary object needs n+1 atoms. For instance, the
-- binary relationship "The _ is _ sometimes" uses the atoms "the", "is"
-- and "sometimes". The atoms on the end, however, can be empty.
data Atom = Atom !ID !String
data Rel1 = Rel1 !RelID !LabID !ID
data Lab1 = Lab1        !LabID !ID !ID
data Rel2 = Rel2 !RelID !LabID !ID !ID
data Lab2 = Lab2        !LabID !ID !ID !ID
data Rel3 = Rel3 !RelID !LabID !ID !ID !ID
data Lab3 = Lab3        !LabID !ID !ID !ID !ID

