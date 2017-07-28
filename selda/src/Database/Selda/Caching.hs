{-# LANGUAGE GADTs, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Selda.Caching
   ( CacheKey
   , cache, cached, invalidate, setMaxItems
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
import Data.IORef
import System.IO.Unsafe
#endif
import Data.Text (Text)
import Database.Selda.SQL (Param (..))
import Database.Selda.Types (TableName)

type CacheKey = (Text, Text, [Param])

#ifdef NO_LOCALCACHE

cache :: Typeable a => [TableName] -> CacheKey -> a -> IO ()
cache _ _ _ = return ()

cached :: Typeable a => CacheKey -> IO (Maybe a)
cached _ = return Nothing

invalidate :: [TableName] -> IO ()
invalidate _ = return ()

setMaxItems :: Int -> IO ()
setMaxItems _ = return ()

#else
instance Hashable Param where
  hashWithSalt s (Param x) = hashWithSalt s x
instance Hashable (Lit a) where
  hashWithSalt s (LText x)     = hashWithSalt s x
  hashWithSalt s (LInt x)      = hashWithSalt s x
  hashWithSalt s (LDouble x)   = hashWithSalt s x
  hashWithSalt s (LBool x)     = hashWithSalt s x
  hashWithSalt s (LDateTime x) = hashWithSalt s x
  hashWithSalt s (LDate x)     = hashWithSalt s x
  hashWithSalt s (LTime x)     = hashWithSalt s x
  hashWithSalt s (LBlob x)     = hashWithSalt s x
  hashWithSalt s (LJust x)     = hashWithSalt s x
  hashWithSalt _ (LNull)       = 0
  hashWithSalt s (LCustom l)   = hashWithSalt s l

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

{-# NOINLINE theCache #-}
theCache :: IORef ResultCache
theCache = unsafePerformIO $ newIORef emptyCache

-- | Cache the given value, with the given table dependencies.
cache :: Typeable a => [TableName] -> CacheKey -> a -> IO ()
cache tns k v = atomicModifyIORef' theCache $ \c -> (cache' tns k v c, ())

cache' :: Typeable a => [TableName] -> CacheKey -> a -> ResultCache -> ResultCache
cache' tns k v rc
  | maxItems rc == 0 = rc
  | prio + 1 < prio  = cache' tns k v (emptyCache {maxItems = maxItems rc})
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
cached :: forall a. Typeable a => CacheKey -> IO (Maybe a)
cached k = atomicModifyIORef' theCache $ cached' k

cached' :: forall a. Typeable a => CacheKey -> ResultCache -> (ResultCache, Maybe a)
cached' k rc = do
  case (maxItems rc, alter updatePrio k (results rc)) of
    (0, _)                  -> (rc, Nothing)
    (_, (Just x, results')) -> (rc' results', fromDynamic x)
    _                       -> (rc, Nothing)
  where
    rc' rs = rc
      { results = rs
      , nextPrio = nextPrio rc + 1
      }
    updatePrio (Just (_, v)) = (Just v, Just (nextPrio rc, v))
    updatePrio _             = (Nothing, Nothing)

-- | Invalidate all items in the per-process cache that depend on
--   the given table.
invalidate :: [TableName] -> IO ()
invalidate tns = atomicModifyIORef' theCache $ \c -> (foldl' (flip invalidate') c tns, ())

invalidate' :: TableName -> ResultCache -> ResultCache
invalidate' tbl rc
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
setMaxItems :: Int -> IO ()
setMaxItems n = atomicModifyIORef' theCache $ \_ -> (setMaxItems' n, ())

setMaxItems' :: Int -> ResultCache
setMaxItems' 0 = emptyCache
setMaxItems' n = emptyCache {maxItems = n}
#endif
