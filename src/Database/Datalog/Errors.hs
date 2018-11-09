{-# LANGUAGE DeriveDataTypeable #-}
module Database.Datalog.Errors ( DatalogError(..) ) where

import Control.Exception
import Data.Text ( Text )
import Data.Typeable

import Database.Datalog.RelationHandle

data DatalogError = SchemaError RelationHandle
                  | RelationExistsError Text
                  | NoRelationError RelationHandle
                  | MissingQueryError
                  | ExtraQueryError
                  | StratificationError
                  | RangeRestrictionViolation
                  | NonVariableInRuleHead
                  | NoVariableBinding Text
                  deriving (Typeable, Show)

instance Exception DatalogError
