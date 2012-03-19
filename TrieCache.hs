{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module TrieCache where

import Life hiding (singleton)
import Misc

import Data.Default
import Data.Map (Map)
import Data.Tuple.All
import qualified Data.Map as M

data NonEmptyTrieCache k time = NonEmptyTrieCache {
	prefix   :: [k],
	here     :: Life time,
	deeper   :: Life time,
	children :: Map k (NonEmptyTrieCache k time)
	} deriving (Eq, Show, Read)

type TrieCache k time = Maybe (NonEmptyTrieCache k time)

instance (PPrint k, PPrint time) => PPrint (TrieCache k time) where pprint = maybe "" pprint
instance (PPrint k, PPrint time) => PPrint (NonEmptyTrieCache k time) where
	pprint = go [] where
		go pre t = pprint fullPre ++ ": " ++ signal ++ "\n" ++ deep where
			fullPre = pre ++ prefix t
			signal  = pprint (here t) ++ "/" ++ pprint (deeper t)
			deep    = M.assocs (children t) >>= \(pre', t') -> go (fullPre ++ [pre']) t'

singleton :: [k] -> Life time -> NonEmptyTrieCache k time
singleton path l = NonEmptyTrieCache path l l def

empty :: TrieCache k time
empty = def

on1 f t = upd1 (f (sel1 t)) t
splitPrefix (p:ps) (p':ps') | p == p' = on1 (p:) (splitPrefix ps ps')
splitPrefix ps ps' = ([], ps, ps')

addLife :: (Ord k, Ord time) => [k] -> Life time -> TrieCache k time -> (TrieCache k time, Life time)
addLife path l = on1 Just . addLife' path l

addLife' path l Nothing = (singleton path l, l)
addLife' path l (Just trie) = case splitPrefix path (prefix trie) of
	(shared, pre , p':ps') -> (NonEmptyTrieCache {
		prefix   = shared,
		here     = if null pre then l else def,
		deeper   = union l (deeper trie),
		children = M.fromList $ (p', trie { prefix = ps' }) : [(p, singleton ps l) | p:ps <- [pre]]
		}, diff l (deeper trie))
	(shared, p:ps, []    ) -> let (trie', l') = addLife' ps l (M.lookup p (children trie)) in (trie {
		deeper   = union l' (deeper trie),
		children = M.insert p trie' (children trie)
		}, diff l' (deeper trie))
	(shared, []  , []    ) -> (trie {
		here     = union l (here trie),
		deeper   = union l (deeper trie)
		}, diff l (deeper trie))

subLife :: (Ord k, Ord time) => [k] -> Life time -> TrieCache k time -> (TrieCache k time, Life time)
subLife path l Nothing = (Nothing, def)
subLife path l t@(Just trie) = case splitPrefix path (prefix trie) of
	(shared, _   , _:_) -> (t, def)
	(shared, p:ps, [] ) -> case (isEmpty (here trie), M.size newChildren) of
		(True, 0) -> res
		(True, 1) -> (Just firstNewChild {
			prefix   = prefix trie ++ (\[(k, t)] -> k : prefix t) (M.assocs newChildren)
			}, diff l' (deeper firstNewChild))
		_ -> (Just trie {
			deeper   = diff (deeper trie) l'',
			children = newChildren
			}, l'')
		where
		res@(t', l')  = subLife ps l (M.lookup p (children trie))
		otherChildren = map deeper . M.elems . M.delete p . children $ trie
		newChildren   = M.update (const t') p (children trie)
		firstNewChild = head (M.elems newChildren)
		l'' = diff l' (unions (here trie : otherChildren))
	(shared, []  , [] ) -> case (isEmpty newHere, M.size (children trie)) of
		(True, 0) -> (Nothing, here trie)
		(True, 1) -> (Just firstChild {
			prefix   = prefix trie ++ (\[(k, t)] -> k : prefix t) (M.assocs (children trie))
			}, diff l (deeper firstChild))
		_ -> (Just trie {
			here     = newHere,
			deeper   = diff (deeper trie) l''
			}, l'')
		where
		newHere       = diff (here trie) l
		otherChildren = map deeper . M.elems . children $ trie
		firstChild    = head       . M.elems . children $ trie
		l'' = diff l (unions (newHere : otherChildren))

deletePrefix :: Ord k => [k] -> NonEmptyTrieCache k time -> TrieCache k time
deletePrefix path trie = case splitPrefix path (prefix trie) of
	(shared,  [] , ps ) -> Just (trie { prefix = ps })
	(shared, p:ps, [] ) -> M.lookup p (children trie) >>= deletePrefix ps
	(shared, _:_ , _:_) -> Nothing

query :: Ord k => [k] -> TrieCache k time -> Life time
query path trie = maybe def deeper (trie >>= deletePrefix path)

assocs :: Ord time => NonEmptyTrieCache k time -> [([k], Life time)]
assocs = assocs' id where
	assocs' pre trie =
		[(pre (prefix trie), here trie) | not . isEmpty . here $ trie] ++
		concatMap (\(k, t) -> assocs' (pre . (prefix trie ++) . (k:)) t) (M.assocs (children trie))
