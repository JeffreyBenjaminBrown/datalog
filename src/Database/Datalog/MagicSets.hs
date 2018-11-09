{-# LANGUAGE BangPatterns #-}
module Database.Datalog.MagicSets ( magicSetsRules, seedDatabase ) where

import Control.Monad ( MonadPlus(..), foldM )
import qualified Control.Monad.Catch as E
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import Data.Sequence ( Seq, (><), ViewL(..) )
import qualified Data.Sequence as S
import Data.Text ( Text )

import Database.Datalog.Adornment
import Database.Datalog.Database
import Database.Datalog.Errors
import Database.Datalog.RelationHandle
import Database.Datalog.Rules


-- FIXME: All references to negated relations must refer to the
-- Rel[FFF] relation version because we don't transform those into
-- versions with bound variables

seedDatabase :: (E.MonadThrow m, Eq a, Hashable a, Show a)
                => Database a
                -> [Rule a]
                -> Query a
                -> [(Text, a)]
                -> m (Database a)
seedDatabase db0 rs (Query (Clause (RelationHandle rname) ts)) bindings = do
  (tup, bs) <- foldM toTuple ([], []) ts
  let magicRel = MagicRelation (BindingPattern (reverse bs)) rname
      r0 = ensureDatabaseRelation db0 magicRel (length tup)
      -- If there is a rule that defines the magic relation, we need
      -- to force the evaluator to evaluate that rule by toggling the
      -- dirty bit (delta table).  We do this by using
      -- addTupleToRelation.  If there is no rule defining the magic
      -- table, we can't do that because the delta bit will never be
      -- toggled off and the evaluator will loop forever.  In that
      -- case, we have to use addTupleToRelation'
      r1 = case any (definesRelation magicRel) rs of
        True -> addTupleToRelation r0 (Tuple (reverse tup))
        False -> addTupleToRelation' r0 (Tuple (reverse tup))
  return $! replaceRelation db0 r1
  where
    toTuple (tacc, bacc) t =
      case t of
        Atom a -> return (a : tacc, B : bacc)
        BindVar name ->
          case lookup name bindings of
            Nothing -> E.throwM (NoVariableBinding name)
            Just v -> return (v : tacc, B : bacc)
        LogicVar _ -> return (tacc, F : bacc)
        FreshVar _ -> return (tacc, F : bacc)
        Anything -> error "Anything should be removed before seedDatabase"
seedDatabase _ _ _ _ = error "Should this be impossible?" -- TODO ?

definesRelation :: RelationHandle -> Rule a -> Bool
definesRelation r (Rule ac _ _) = adornedClauseRelation ac == r

-- | Returns the rules generated by the magic sets transformation
--
-- If there are no BoundVars or Atoms in the query, don't perform the
-- transformation since it won't help much.
--
-- Note that performing the simple magic sets transformation on a
-- negated literal can break stratification.  For now, this
-- implementation will not compute magic sets for negated literals.
-- That is, if a relation appears as a negated literal, do not perform
-- the magic transformation on it.  It isn't quite clear to me if it
-- is just literals appearing negated or all literals used to define
-- literals appearing negated.
--
-- There is an algorithm in
--
-- > I. Balbin, G.S. Port, K. Ramamohanarao, K. Meenakshi, Efficient bottom-up computation of queries on stratified databases, The Journal of Logic Programming, Volume 11, Issues 3–4, October–November 1991, Pages 295-344, ISSN 0743-1066, 10.1016/0743-1066(91)90030-S.
-- > (http://www.sciencedirect.com/science/article/pii/074310669190030S)
--
-- that handles magic for negated literals.
magicSetsRules :: (E.MonadThrow m, Hashable a, Eq a, Show a)
                  => Query a -- ^ The goal query
                  -> [(Clause a, [Literal Clause a])] -- ^ The user-provided rules
                  -> m [Rule a]
magicSetsRules q rs =
  -- mapM adornRule rs
  transformRules (S.singleton (queryPattern q)) mempty
  where
    -- These cannot be transformed
    negatedRelations = foldr collectNegatedRelations mempty rs
    -- Any relations in this list are inferred by rules and are
    -- therefore eligible for the magic transformation (relations
    -- in the fact database are not).
    rawRules = foldr groupRules mempty rs
    groupRules r = HM.insertWith (++) (clauseRelation (fst r)) [r]
    inferredRelations = HS.fromList $ HM.keys rawRules

    isInferred :: QueryPattern -> Bool
    isInferred p = HS.member (queryPatternRelation p) inferredRelations

    transformRules !worklist !generated =
      case S.viewl worklist of
        EmptyL -> do
          let filteredRules = concat (HM.elems generated)
              recPreds = HS.fromList $ map queryPatternRelation (HM.keys generated)
              magicFilterTables = concatMap (toMagicFilterTable recPreds) filteredRules
          mapM adornRule (map fst filteredRules ++ magicFilterTables)
        elt :< rest ->
          case HM.lookup elt generated of
            -- Already processed this binding pattern
            Just _ -> transformRules rest generated
            Nothing -> do
              let matchingRules = fromMaybe (error "No rules for pattern") $ HM.lookup (queryPatternRelation elt) rawRules
              (magic, newWork) <- foldM (magicTransform elt) (mempty, mempty) matchingRules
              transformRules (rest >< newWork) (HM.insert elt magic generated)

-- The QueryPattern doesn't affect the adornments added for the
-- sideways information passing strategy (for that, the terms in the
-- head area *always* bound).  The QueryPattern is separate and is
-- only used to compute other QueryPatterns for the worklist and to
-- determine whether or not magic needs to be applied.
    magicTransform :: (E.MonadThrow m, Hashable a, Eq a, Show a)
                      => QueryPattern
                      -> ([((Clause a, [Literal Clause a]), [QueryPattern])], Seq QueryPattern)
                      -> (Clause a, [Literal Clause a])
                      -> m ([((Clause a, [Literal Clause a]), [QueryPattern])], Seq QueryPattern)
    magicTransform bp (newRules, work) rawRule@(c, lits) = do
      let hasB = hasBinding bp
          isNeg = HS.member (clauseRelation c) negatedRelations
          bodyBindingPattern = reverse $ snd $ foldl' bindVars (patternToInitialMap bp c, []) lits
          -- adornedLits = zip lits bodyBindingPattern
          newDeps = filter isInferred bodyBindingPattern
          newWork = work >< S.fromList newDeps
      case not hasB || isNeg of
        True -> do
          -- If a rule has no bindings in the head (or has a
          -- negation), we don't do the magic transformation.  We
          -- still need to make sure all of its reachable literals are
          -- processed, though.
          return ((rawRule, []) : newRules, newWork)
        False -> do
          let (mf, mp) = buildMagicFilter bp c
          return (((c, mf : lits), mp : bodyBindingPattern) : newRules, newWork)

-- | For each literal referencing a recursive relation (even if it is
-- recursive in a different rule), generate a magic filter table
-- definition rule for it.
toMagicFilterTable :: (Eq a)
                      => HashSet RelationHandle
                      -> ((Clause a, [Literal Clause a]), [QueryPattern])
                      -> [(Clause a, [Literal Clause a])]
toMagicFilterTable ps ((_, lits), qps) =
  map (buildMagicFilterRule lits) (filter (isRecPred . fst) body)
  where
    body = zip lits qps
    isRecPred l =
      case l of
        Literal (Clause r _) -> r `HS.member` ps
        _ -> False

-- | Take a binding pattern and a rule head and create its magic
-- filter literal.  The magic filter literal is the head clause
-- changed to reference a magic version of the same relation and with
-- the free columns deleted.
buildMagicFilter :: QueryPattern -> Clause a -> (Literal Clause a, QueryPattern)
buildMagicFilter qp (Clause (RelationHandle t) ts) =
  (Literal (Clause mrel retainedTs), QueryPattern mrel (BindingPattern (map (const F) retainedTs)))
  where
    mrel = MagicRelation bp t
    bp = queryPatternBindings qp
    retainedTs = takeBoundTerms qp ts
buildMagicFilter _ _ = error "Cannot have a magic relation yet"

takeBoundTerms :: QueryPattern -> [Term a] -> [Term a]
takeBoundTerms (QueryPattern _ qp) ts =
  map snd retainedTuples
  where
    allTuples = zip (bindingPattern qp) ts
    retainedTuples = filter ((==B) . fst) allTuples

-- | For each occurrence of the head clause in a literal, generate a
-- rule defining the magic filter.
--
-- To do that for occurrence O,
--
--  1) Delete everything to the right of O in the body
--
--  2) Turn O into a magic clause and delete its free columns
--
--  3) Replace the head with O
buildMagicFilterRule :: (Eq a)
                        => [Literal Clause a]
                        -> (Literal Clause a, QueryPattern)
                        -> (Clause a, [Literal Clause a])
buildMagicFilterRule lits (lc@(Literal c), qp) =
  let retainedLits = takeWhile (/= lc) lits
      retainedTerms = takeBoundTerms qp (clauseTerms c)
      RelationHandle relName = clauseRelation c
      h = Clause (MagicRelation (queryPatternBindings qp) relName) retainedTerms
  in (h, retainedLits)
buildMagicFilterRule _ _ = error "Should this be impossible?" -- TODO ?

bindVars :: (Eq a, Hashable a)
            => (HashSet (Term a), [QueryPattern])
            -> Literal Clause a
            -> (HashSet (Term a), [QueryPattern])
bindVars acc@(alreadyBound, patts) l =
  case l of
    ConditionalClause _ _ _ _ -> acc
    Literal (Clause r ts) ->
      let (binds, qp) = foldl' bindVar (alreadyBound, []) ts
      in (binds, QueryPattern r (BindingPattern (reverse qp)) : patts)
    -- For now, we treat all variables in a negated literal as Free
    -- because we don't want to generate any magic clauses for them
    -- (that can break stratification).  Treating them all as free
    -- here gets them properly skipped later.
    NegatedLiteral (Clause r ts) ->
      let qp = map (const F) ts
      in (alreadyBound, QueryPattern r (BindingPattern qp) : patts)
  where
    bindVar (bindings, bs) t =
      case t `HS.member` bindings of
        True -> (bindings, B : bs)
        False ->
          case t of
            LogicVar _ -> (HS.insert t bindings, F : bs)
            Anything -> error "Wildcard variables should have been rewritten already"
            FreshVar _ -> (HS.insert t bindings, F : bs)
            BindVar _ -> (bindings, B : bs)
            Atom _ -> (bindings, B : bs)


patternToInitialMap :: (Eq a, Hashable a) => QueryPattern -> Clause a -> HashSet (Term a)
patternToInitialMap qp (Clause _ ts) =
  HS.fromList $ takeBoundTerms qp ts

data QueryPattern = QueryPattern { queryPatternRelation :: RelationHandle
                                 , queryPatternBindings :: BindingPattern
                                 }
                  deriving (Eq, Show)

instance Hashable QueryPattern where
  hashWithSalt s (QueryPattern r bs) =
    s `hashWithSalt` r `hashWithSalt` bs

hasBinding :: QueryPattern -> Bool
hasBinding (QueryPattern _ bs) = any (==B) (bindingPattern bs)

queryPattern :: Query a -> QueryPattern
queryPattern (Query c) =
  QueryPattern (clauseRelation c) $ BindingPattern (map toBinding (clauseTerms c))
  where
    toBinding t =
      case t of
        Atom _ -> B
        BindVar _ -> B
        LogicVar _ -> F
        Anything -> F
        FreshVar _ -> F

-- If the input query binding doesn't have any bound elements, the
-- rule gets no magic.

-- FIXME: This would be better if dead rules couldn't affect it...
-- a dead rule with a negation will be a problem.
collectNegatedRelations :: (Clause a, [Literal Clause a])
                           -> HashSet RelationHandle
                           -> HashSet RelationHandle
collectNegatedRelations (_, cs) acc =
  foldr addIfNegated acc cs
  where
    addIfNegated (NegatedLiteral (Clause h _)) s = HS.insert h s
    addIfNegated _ s = s

-- If the rule ends up with multiple binding patterns for the
-- recursive rule, the rule needs to be split.  This means that, for
-- each binding pattern, the full set of rules defining that relation
-- must be duplicated

-- If the query has a bound literal, that influences the rules it
-- corresponds to.  Other rules are not affected.  Those positions
-- bound in the query are bound in the associated rules.
--
-- Note: all variables in the head must appear in the body
adornRule :: (E.MonadThrow m, Eq a, Hashable a)
              => (Clause a, [Literal Clause a]) -> m (Rule a)
adornRule (hd, lits) = do
  (vmap, lits') <- mapAccumM adornLiteral mempty lits
  (allVars, Literal hd') <- adornLiteral vmap (Literal hd)
  let headVars = HS.fromList (clauseTerms hd)
  -- FIXME: This test isn't actually strict enough.  All head vars
  -- must appear in a non-negative literal
  case headVars `HS.difference` (HS.fromList (HM.keys allVars)) == mempty of
    True -> return $! Rule hd' lits' allVars
    False -> E.throwM RangeRestrictionViolation

adornLiteral :: (E.MonadThrow m, Eq a, Hashable a)
                => HashMap (Term a) Int
                -> Literal Clause a
                -> m (HashMap (Term a) Int, Literal AdornedClause a)
adornLiteral boundVars l =
  case l of
    Literal c -> adornClause Literal c
    NegatedLiteral c -> adornClause NegatedLiteral c
    ConditionalClause cid f ts _ ->
      return (boundVars, ConditionalClause cid f ts boundVars)
  where
    adornClause constructor (Clause p ts) = do
      (bound', ts') <- mapAccumM adornTerm boundVars ts
      let c' = constructor $ AdornedClause p ts'
      return (bound', c')
    adornTerm bvs t =
      case t of
        BindVar _ -> error "Bind variables are only allowed in queries"
        Anything -> error "Anything should have been removed already"
        -- Atoms are always bound
        Atom _ -> return (bvs, (t, BoundAtom))
        LogicVar _ ->
          -- The first occurrence is Free, while the rest are Bound
          case HM.lookup t bvs of
            Just ix -> return (bvs, (t, Bound ix))
            Nothing ->
              let ix = HM.size bvs
              in return (HM.insert t ix bvs, (t, Free ix))
        FreshVar _ ->
          let ix = HM.size bvs
          in return (HM.insert t ix bvs, (t, Free ix))

-- Helpers missing from the standard libraries

{-# INLINE mapAccumM #-}
-- | Monadic mapAccumL
mapAccumM :: (Monad m, MonadPlus p) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, p y)
mapAccumM _ z [] = return (z, mzero)
mapAccumM f z (x:xs) = do
  (z', y) <- f z x
  (z'', ys) <- mapAccumM f z' xs
  return (z'', return y `mplus` ys)
