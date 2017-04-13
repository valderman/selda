{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Selda.Caching
   ( ResultCache, CacheKey
    , emptyCache, cache, cached, invalidate, setMaxItems, maxItems
   ) where
import Prelude hiding (lookup)
import Data.Hashable
import Data.HashPSQ
import Data.IORef
import Data.List (foldl')
import Data.Text (Text)
import System.IO.Unsafe
import Database.Selda.SQL (Param (..))
import Database.Selda.SqlType
import Database.Selda.Types (TableName)

type CacheKey = (Text, [Param])

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

-- TODO: add dependency on unordered-containers as well instead of abusing
-- PSQs as hash maps?
data ResultCache = ResultCache
  { -- | Query to result mapping.
    results  :: !(HashPSQ CacheKey Int [[SqlValue]])
    -- | Table to query mapping, for keeping track of which queries depend on
    --   which tables.
  , deps     :: !(HashPSQ TableName () (HashPSQ CacheKey () ()))
    -- | Items currently in cache.
  , items    :: !Int
    -- | Max number of items in cache.
  , maxItems :: !Int
    -- | Next cache prio to use.
  , nextPrio :: !Int
  } deriving Show

{-# NOINLINE resultCache #-}
resultCache :: IORef ResultCache
resultCache = unsafePerformIO $ newIORef emptyCache

emptyCache :: ResultCache
emptyCache = ResultCache
  { results  = empty
  , deps     = empty
  , items    = 0
  , maxItems = 0
  , nextPrio = 0
  }

-- | Cache the given value, with the given table dependencies.
cache :: [TableName] -> CacheKey -> [[SqlValue]] -> ResultCache -> ResultCache
cache tns k v rc
  | maxItems rc == 0 = rc
  | prio + 1 < prio = cache tns k v $ rc
    { nextPrio = 0
    , results = empty
    , deps = empty
    , items = 0
    }
  | items rc >= maxItems rc = rc
    { results  = insert k prio v (deleteMin $ results rc)
    , deps     = foldl' (\m tn -> snd $ alter (addTbl k) tn m) (deps rc) tns
    , nextPrio = prio + 1
    }
  | otherwise = rc
    { results = insert k (nextPrio rc) v (results rc)
    , deps     = foldl' (\m tn -> snd $ alter (addTbl k) tn m) (deps rc) tns
    , items    = items rc + 1
    , nextPrio = prio + 1
    }
  where
    prio = nextPrio rc
    addTbl key (Just (p, ks)) = ((), Just (p, insert key () () ks))
    addTbl key Nothing        = ((), Just ((), singleton key () ()))

-- | Get the cached value for the given key, if it exists.
cached :: CacheKey -> ResultCache -> Maybe [[SqlValue]]
cached k rc = do
  if maxItems rc == 0
    then Nothing
    else snd <$> lookup k (results rc)

-- | Invalidate all items in cache that depend on the given table.
invalidate :: TableName -> ResultCache -> ResultCache
invalidate tbl rc
  | maxItems rc == 0 =
    rc
  | otherwise = rc
    { results = results'
    , deps    = deps'
    , items   = items rc - length ks
    }
  where
    ks = maybe [] (keys . snd) $ lookup tbl (deps rc)
    results' = foldl' (flip delete) (results rc) ks
    deps' = delete tbl (deps rc)

-- | Set the maximum number of items allowed in the cache.
setMaxItems :: Int -> ResultCache -> ResultCache
setMaxItems n rc = rc {maxItems = n}
