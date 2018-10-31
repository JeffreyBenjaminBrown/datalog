-- | This is simpler than but exactly parallel to
-- the first query in tests/WorksForTest.hs
--
-- PITFALL: Search for the use of BindVar in WorksForTest
-- to see how to run more flexible queries.

{-# LANGUAGE OverloadedStrings #-}
import Database.Datalog


mdb :: Maybe (Database String)
mdb = makeDatabase $ do
  hasFur <- addRelation "has fur" 1
  mapM_ (assertFact hasFur) [ [ "Garfield" ]
                            , [ "Odey" ]
                            ]
  hatesRaisins <- addRelation "hates raisins" 1
  assertFact hatesRaisins ["Garfield"]

db :: Database String
Just db = mdb

qb :: QueryBuilder Maybe String (Query String)
qb = do
  hasFur <- relationPredicateFromName "has fur"
  hatesRaisins <- relationPredicateFromName "hates raisins"
  shouldReturnOdey <- inferencePredicate "should return Odey"
  let x = LogicVar "X"
  (shouldReturnOdey, [x]) |- [ lit hasFur [x]
                             , negLit hatesRaisins [x]
                             ]
  issueQuery shouldReturnOdey [x]
    -- PITFALL: Search for the use of BindVar in WorksForTest
    -- to see how to run more flexible queries.

qp :: QueryPlan String
Just qp = buildQueryPlan db qb

go :: [[[String]]]
go = executeQueryPlan qp db []
