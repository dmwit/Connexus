-- boilerplate {{{1
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, TypeSynonymInstances #-}
module TrieCache where

import Misc

import Data.Default
import Data.Map (Map)
import Data.Monoid
import Data.Tuple.All
import qualified Data.Map as M

-- types {{{1
-- A trie used to store elements of two commutative monoids and cache the two
-- (monoid) sums of all the values that appear under a given prefix.
data NonEmptyTrieCache k v = NonEmptyTrieCache {
	prefix   :: [k],
	value    :: Maybe v,
	cache    :: v,
	children :: Map k (NonEmptyTrieCache k v)
	} deriving (Eq, Show, Read)

type TrieCache k v = Maybe (NonEmptyTrieCache k v)

-- maintaining invariants {{{1
-- naming {{{2
{-
We maintain two invariants that are not reflected in the type system:
1. (compressed) If "value" is "Nothing", then there are at least two children.
2. (coherent) The "cache" is the (monoid) sum of the "value" (if any) and the
   "cache"s of the children. (Hence, it is the sum of all "value"s in the
   entire trie.)
There are 16 potentially useful functions:
1. checking the invariant or reinstating the invariant,
2. working on the compressed or coherent invariant,
3. working on non-empty tries or possibly-empty tries, and
4. "shallow" (the invariant applies to the current node) or "deep" (the
   invariant applies to all nodes in the trie) versions.
The grammar for function names in this section reflects these choices (with
both the decisions to be made and the possible outcomes of those decisions in
the same order as above):
name ::= ("check" | "inv") ("Compressed" | "Coherent") ("NE" | "") ("" | "'")
-}

-- check {{{2
checkCompressedNE t = maybe True (const (M.size (children t) >= 2)) (value t)
checkCoherentNE   t = cache t == mconcat (caches t)

checkDeepNE p t    = p t && all (checkDeepNE p) (M.elems (children t))
checkCompressedNE' = checkDeepNE checkCompressedNE
checkCoherentNE'   = checkDeepNE checkCoherentNE

checkPossiblyEmpty = maybe True
checkCompressed    = checkPossiblyEmpty checkCompressedNE
checkCoherent      = checkPossiblyEmpty checkCoherentNE
checkCompressed'   = checkPossiblyEmpty checkCompressedNE'
checkCoherent'     = checkPossiblyEmpty checkCoherentNE'

-- reinstate {{{2
invCompressedNE t@(NonEmptyTrieCache { value = Nothing, children = c }) = case (M.size c, M.assocs c) of
	(0, _)         -> Nothing
	(1, [(k, t')]) -> Just t' { prefix = prefix t ++ k : prefix t' }
	_              -> Just t
invCompressedNE t = Just t
invCoherentNE   t = t { cache = mconcat (caches t) }

invCompressedNE' t = invCompressedNE t { children = M.mapMaybe invCompressedNE' (children t) }
invCoherentNE'   t = invCoherentNE   t { children = fmap       invCoherentNE'   (children t) }

invPossiblyEmpty = (=<<)
invCompressed    = invPossiblyEmpty invCompressedNE
invCoherent      = invPossiblyEmpty (Just . invCoherentNE)
invCompressed'   = invPossiblyEmpty invCompressedNE'
invCoherent'     = invPossiblyEmpty (Just . invCoherentNE')

-- PPrint {{{1
instance (PPrint k, PPrint v) => PPrint (TrieCache k v) where pprint = maybe "" pprint
instance (PPrint k, PPrint v) => PPrint (NonEmptyTrieCache k v) where
	pprint = go [] where
		go pre t = pprint fullPre ++ ": " ++ signal ++ "\n" ++ deep where
			fullPre = pre ++ prefix t
			signal  = maybe "(branch only)" pprint (value t) ++ "/" ++ pprint (cache t)
			deep    = M.assocs (children t) >>= \(pre', t') -> go (fullPre ++ [pre']) t'

-- creation {{{1
singletonNE :: [k] -> v -> NonEmptyTrieCache k v
singletonNE path v = NonEmptyTrieCache path (Just v) v def

singleton :: [k] -> v -> TrieCache k v
singleton path v = Just (singletonNE path v)

empty :: TrieCache k v
empty = def

unionWithNE :: (Ord k, Monoid v) => (v -> v -> v) -> NonEmptyTrieCache k v -> NonEmptyTrieCache k v -> NonEmptyTrieCache k v
unionWithNE f trie trie' = invCoherentNE NonEmptyTrieCache {
	prefix   = shared,
	value    = unionWithMaybe f $ [value trie | null path] ++ [value trie' | null path'],
	cache    = error "The impossible happened: the cache wasn't overwritten in unionWithNE.",
	children = newChildren
	} where
	(shared, path, path') = splitPrefix (prefix trie) (prefix trie')
	newChildren = case (path, path') of
		([]  , []    ) -> M.unionWith  (unionWithNE f) (children trie) (children trie')
		(p:ps, []    ) -> M.insertWith (unionWithNE f) p  trie  { prefix = ps } (children trie')
		([]  , p':ps') -> M.insertWith (unionWithNE f) p' trie' { prefix = ps'} (children trie )
		(p:ps, p':ps') -> M.fromList [(p, trie { prefix = ps }), (p', trie' { prefix = ps' })]

unionMNE :: (Ord k, Monoid v) => NonEmptyTrieCache k v -> NonEmptyTrieCache k v -> NonEmptyTrieCache k v
unionMNE trie trie' = NonEmptyTrieCache {
	prefix   = shared,
	value    = mconcat $ [value trie | null path] ++ [value trie' | null path'],
	cache    = cache trie `mappend` cache trie',
	children = newChildren
	} where
	(shared, path, path') = splitPrefix (prefix trie) (prefix trie')
	newChildren = case (path, path') of
		([]  , []    ) -> M.unionWith  unionMNE (children trie) (children trie')
		(p:ps, []    ) -> M.insertWith unionMNE p  trie  { prefix = ps  } (children trie')
		([]  , p':ps') -> M.insertWith unionMNE p' trie' { prefix = ps' } (children trie )
		(p:ps, p':ps') -> M.fromList [(p, trie { prefix = ps }), (p', trie' { prefix = ps' })]

adjustNE :: (Ord k, Monoid v) => (v -> v) -> [k] -> NonEmptyTrieCache k v -> NonEmptyTrieCache k v
adjustNE f = go where
	go path trie = case splitPrefix path (prefix trie) of
		(shared, []  , []) -> invCoherentNE trie { value = fmap f (value trie) }
		(shared, k:ks, []) -> invCoherentNE trie { children = M.adjust (go ks) k (children trie) }
		_ -> trie

deleteNE :: (Ord k, Monoid v) => [k] -> NonEmptyTrieCache k v -> TrieCache k v
deleteNE path trie = case splitPrefix path (prefix trie) of
	(_, []  , []) -> invBoth trie { value = Nothing }
	(_, p:ps, []) -> invBoth trie { children = M.update (deleteNE ps) p (children trie) }
	(_, _   , _ ) -> Just trie
	where invBoth = invCoherent . invCompressedNE

deleteSubTrieNE :: (Ord k, Monoid v) => [k] -> NonEmptyTrieCache k v -> TrieCache k v
deleteSubTrieNE path trie = case splitPrefix path (prefix trie) of
	(_, []  , _ ) -> Nothing
	(_, p:ps, []) -> invBoth trie { children = M.update (deleteSubTrieNE ps) p (children trie) }
	(_, _   , _ ) -> Just trie
	where invBoth = invCoherent . invCompressedNE

insertWithNE f = (unionWithNE f .) . singletonNE
insertMNE = (unionMNE .) . singletonNE
insertNE = insertWithNE const

insertPossiblyEmpty f path v = Just . maybe (singletonNE path v) (f path v)
insertWith f = insertPossiblyEmpty (insertWithNE f)
insertM      = insertPossiblyEmpty insertMNE
insert       = insertPossiblyEmpty insertNE

adjust = (fmap .) . adjustNE
delete = (=<<) . deleteNE
deleteSubTrie = (=<<) . deleteSubTrieNE

-- query {{{1
descendNE :: Ord k => [k] -> NonEmptyTrieCache k v -> TrieCache k v
descendNE path trie = case splitPrefix path (prefix trie) of
	(shared, []  , ps ) -> Just (trie { prefix = ps })
	(shared, p:ps, [] ) -> M.lookup p (children trie) >>= descendNE ps
	(shared, _:_ , _:_) -> Nothing

descend :: Ord k => [k] -> TrieCache k v -> TrieCache k v
descend path trie = trie >>= descendNE path

query :: (Ord k, Default v) => [k] -> TrieCache k v -> v
query path = maybe def cache . descend path

assocsNE :: NonEmptyTrieCache k v -> [([k], v)]
assocsNE = go id where
	go pre trie =
		[(pre (prefix trie), v) | Just v <- [value trie]] ++
		concatMap (\(k, t) -> go (pre . (prefix trie ++) . (k:)) t) (M.assocs (children trie))

assocs :: TrieCache k v -> [([k], v)]
assocs = maybe def assocsNE

-- misc {{{1
on1 f t = upd1 (f (sel1 t)) t

splitPrefix (p:ps) (p':ps') | p == p' = on1 (p:) (splitPrefix ps ps')
splitPrefix ps ps' = ([], ps, ps')

caches t = [v | Just v <- [value t]] ++ [cache c | c <- M.elems (children t)]

-- kind of like mconcat, except with f instead of mappend
unionWithMaybe f vs = case [v | Just v <- vs] of
	[]     -> Nothing
	[m, n] -> Just (f m n)
	(m:_)  -> Just m
