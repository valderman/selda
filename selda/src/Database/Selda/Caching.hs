{-# LANGUAGE GADTs, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Selda.Caching
   ( ResultCache, CacheKey
    , emptyCache, cache, cached, invalidate, setMaxItems, maxItems
   ) where
import Prelude hiding (lookup)
import Data.Dynamic
#ifndef NO_LOCALCACHE
import Data.Hashable
import Data.HashPSQ
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl')
import Database.Selda.SqlType
#endif
import Data.Text (Text)
import Database.Selda.SQL (Param (..))
import Database.Selda.Types (TableName)

type CacheKey = (Text, [Param])

#ifdef NO_LOCALCACHE

data ResultCache = ResultCache

emptyCache :: ResultCache
emptyCache = ResultCache

cache :: Typeable a => [TableName] -> CacheKey -> a -> ResultCache -> ResultCache
cache _ _ _ _ = ResultCache

cached :: forall a. Typeable a => CacheKey -> ResultCache -> (Maybe a, ResultCache)
cached _ _ = (Nothing, ResultCache)

invalidate :: TableName -> ResultCache -> ResultCache
invalidate _ _ = ResultCache

setMaxItems :: Int -> ResultCache -> ResultCache
setMaxItems _ _ = ResultCache

maxItems :: ResultCache -> Int
maxItems _ = 0

#else

instance Hashable Param where
  hashWithSalt s (Param x) = hashWithSalt s x
instance Hashable (Lit a) where
  hashWithSalt s (LitS x)    = hashWithSalt s x
  hashWithSalt s (LitI x)    = hashWithSalt s x
  hashWithSalt s (LitD x)    = hashWithSalt s x
  hashWithSalt s (LitB x)    = hashWithSalt s x
  hashWithSalt s (LitTS x)   = hashWithSalt s x
  hashWithSalt s (LitDate x) = hashWithSalt s x
  hashWithSalt s (LitTime x) = hashWithSalt s x
  hashWithSalt s (LitJust x) = hashWithSalt s x
  hashWithSalt _ (LitNull)   = 0

data ResultCache = ResultCache
  { -- | Query to result mapping.
    results  :: !(HashPSQ CacheKey Int Dynamic)
    -- | Table to query mapping, for keeping track of which queries depend on
    --   which tables.
  , deps     :: !(M.HashMap TableName (S.HashSet CacheKey))
    -- | Items currently in cache.
  , items    :: !Int
    -- | Max number of items in cache.
  , maxItems :: !Int
    -- | Next cache prio to use.
  , nextPrio :: !Int
  } deriving Show

emptyCache :: ResultCache
emptyCache = ResultCache
  { results  = empty
  , deps     = M.empty
  , items    = 0
  , maxItems = 0
  , nextPrio = 0
  }

-- | Cache the given value, with the given table dependencies.
cache :: Typeable a => [TableName] -> CacheKey -> a -> ResultCache -> ResultCache
cache tns k v rc
  | maxItems rc == 0 = rc
  | prio + 1 < prio  = cache tns k v (emptyCache {maxItems = maxItems rc})
  | otherwise        = rc
    { results  = insert k prio v' results'
    , deps     = foldl' (\m tn -> M.alter (addTbl k) tn m) (deps rc) tns
    , nextPrio = prio + 1
    , items    = items'
    }
  where
    v' = toDyn v
    prio = nextPrio rc
    -- evict LRU element before inserting if full
    (items', results')
      | items rc >= maxItems rc = (items rc, deleteMin $ results rc)
      | otherwise               = (items rc + 1, results rc)
    addTbl key (Just ks) = Just (S.insert key ks)
    addTbl key Nothing   = Just (S.singleton key)

-- | Get the cached value for the given key, if it exists.
cached :: forall a. Typeable a => CacheKey -> ResultCache -> (Maybe a, ResultCache)
cached k rc = do
  case (maxItems rc, alter updatePrio k (results rc)) of
    (0, _)                  -> (Nothing, rc)
    (_, (Just x, results')) -> (fromDynamic x, rc' results')
    _                       -> (Nothing, rc)
  where
    rc' rs = rc
      { results = rs
      , nextPrio = nextPrio rc + 1
      }
    updatePrio (Just (_, v)) = (Just v, Just (nextPrio rc, v))
    updatePrio _             = (Nothing, Nothing)

-- | Invalidate all items in cache that depend on the given table.
invalidate :: TableName -> ResultCache -> ResultCache
invalidate tbl rc
  | maxItems rc == 0 = rc
  | otherwise        = rc
    { results = results'
    , deps    = deps'
    , items   = items rc - length ks
    }
  where
    ks = maybe S.empty id $ M.lookup tbl (deps rc)
    results' = S.foldl' (flip delete) (results rc) ks
    deps' = M.delete tbl (deps rc)

-- | Set the maximum number of items allowed in the cache.
setMaxItems :: Int -> ResultCache -> ResultCache
setMaxItems 0 _  = emptyCache
setMaxItems n rc = emptyCache {maxItems = n}
#endif
