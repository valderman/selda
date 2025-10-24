{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Building and executing prepared statements.
module Database.Selda.Prepared (Preparable, Prepare, prepared) where
import Database.Selda.Backend.Internal
    ( Lit(LCustom),
      SqlType(sqlType),
      SqlTypeRep,
      Param(..),
      MonadSelda(Backend, withConnection),
      SeldaBackend(ppConfig, runPrepared, backendId, prepareStmt),
      SeldaConnection(connBackend, connStmts),
      SeldaStmt(SeldaStmt, stmtHandle, stmtParams, stmtText),
      StmtID(..),
      BackendID,
      freshStmtId,
      withBackend )
import Database.Selda.Column ( Exp(Lit), Col(..) )
import Database.Selda.Compile
    ( Result, Res, compileWith, buildResult )
import Database.Selda.Query.Type ( Query )
import Database.Selda.SQL (param, paramType)
import Control.Exception ( Exception, try, throw, mask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.IntMap as M
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.Proxy ( Proxy(..) )
import Data.Text (Text)
import Data.Typeable ( Typeable )
import System.IO.Unsafe ( unsafePerformIO )

data Placeholder = Placeholder Int
  deriving Show
instance Exception Placeholder

-- | Index of first argument parameter to a query.
firstParamIx :: Int
firstParamIx = 0

-- | Result type of a monadic computation.
type family ResultT f where
  ResultT (a -> b) = ResultT b
  ResultT (m a)    = a

type family Equiv q f where
  Equiv (Col s a -> q) (a -> f) = Equiv q f
  Equiv (Query s a)    (m [b])  = (Res a ~ b, Backend m ~ s)

type CompResult = (Text, [Either Int Param], [SqlTypeRep])

class Preparable q where
  -- | Prepare the query and parameter list.
  mkQuery :: MonadSelda m
          => Int -- ^ Next argument index.
          -> q   -- ^ The query.
          -> [SqlTypeRep] -- ^ The list of param types so far.
          -> m CompResult

-- | Some parameterized query @q@ that can be prepared into a function @f@
--   in some @MonadSelda@.
class Prepare q f where
  -- | Build the function that prepares and execute the query.
  mkFun :: Preparable q
        => IORef (Maybe (BackendID, CompResult))
        -> StmtID
        -> q
        -> [Param]
        -> f

instance (SqlType a, Prepare q b) => Prepare q (a -> b) where
  mkFun ref sid qry ps x = mkFun ref sid qry (param x : ps)

instance (Typeable a, MonadSelda m, a ~ Res (ResultT q), Result (ResultT q)) =>
         Prepare q (m [a]) where
  -- This function uses read/writeIORef instead of atomicModifyIORef.
  -- For once, this is actually safe: the IORef points to a single compiled
  -- statement, so the only consequence of a race between the read and the write
  -- is that the statement gets compiled (note: NOT prepared) twice.
  mkFun ref (StmtID sid) qry arguments = withConnection $ \conn -> do
    let backend = connBackend conn
        args = reverse arguments
    stmts <- liftIO $ readIORef (connStmts conn)
    case M.lookup sid stmts of
      Just stm -> do
        -- Statement already prepared for this connection; just execute it.
        liftIO $ runQuery conn stm args
      _ -> do
        -- Statement wasn't prepared for this connection; check if it was at
        -- least previously compiled for this backend.
        compiled <- liftIO $ readIORef ref
        (q, params, reps) <- case compiled of
          Just (bid, comp) | bid == backendId backend -> do
            return comp
          _ -> do
            comp <- mkQuery firstParamIx qry []
            liftIO $ writeIORef ref (Just (backendId backend, comp))
            return comp

        -- Prepare and execute
        liftIO $ mask $ \restore -> do
          hdl <- prepareStmt backend (StmtID sid) reps q
          let stm = SeldaStmt
                { stmtHandle = hdl
                , stmtParams = params
                , stmtText = q
                }
          atomicModifyIORef' (connStmts conn) $ \m -> (M.insert sid stm m, ())
          restore $ runQuery conn stm args
    where
      runQuery conn stm args = do
        let ps = replaceParams (stmtParams stm) args
            hdl = stmtHandle stm
        res <- runPrepared (connBackend conn) hdl ps
        return $ map (buildResult (Proxy :: Proxy (ResultT q))) (snd res)

instance (SqlType a, Preparable b) => Preparable (Col s a -> b) where
  mkQuery n f ts = mkQuery (n+1) (f x) (t : ts)
    where
      t = sqlType (Proxy :: Proxy a)
      x = One $ Lit $ LCustom t (throw (Placeholder n) :: Lit a)

instance Result a => Preparable (Query s a) where
  mkQuery _ q types = withBackend $ \b -> do
    case compileWith (ppConfig b) q of
      (q', ps) -> do
        (ps', types') <- liftIO $ inspectParams (reverse types) ps
        return (q', ps', types')

-- | Create a prepared Selda function. A prepared function has zero or more
--   arguments, and will get compiled into a prepared statement by the first
--   backend to execute it. Any subsequent calls to the function for the duration
--   of the connection to the database will reuse the prepared statement.
--
--   Preparable functions are of the form
--   @(SqlType a, SqlType b, ...) => Col s a -> Col s b -> ... -> Query s r@.
--   The resulting prepared function will be of the form
--   @MonadSelda m => a -> b -> ... -> m [Res r]@.
--   Note, however, that when using @prepared@, you must give a concrete type
--   for @m@ due to how Haskell's type class resolution works.
--
--   Prepared functions rely on memoization for just-in-time preparation and
--   caching. This means that if GHC accidentally inlines your prepared function,
--   it may get prepared twice.
--   While this does not affect the correctness of your program, and is
--   fairly unlikely to happen, if you want to be absolutely sure that your
--   queries aren't re-prepared more than absolutely necessary,
--   consider adding a @NOINLINE@ annotation to each prepared function.
--
--   Note that when using a constrained backend type variable (i.e.
--   @foo :: Bar b => SeldaM b [Int]@), optimizations must be enabled for
--   prepared statements to be effective.
--
--   A usage example:
--
-- > persons :: Table (Text, Int)
-- > (persons, name :*: age) = tableWithSelectors "ages" [name :- primary]
-- >
-- > {-# NOINLINE ageOf #-}
-- > ageOf :: Text -> SeldaM [Int]
-- > ageOf = prepared $ \n -> do
-- >   person <- select ages
-- >   restrict $ (person!name .== n)
-- >   return age
{-# NOINLINE prepared #-}
prepared :: (Preparable q, Prepare q f, Equiv q f) => q -> f
prepared q = unsafePerformIO $ do
  ref <- newIORef Nothing
  sid <- freshStmtId
  return $ mkFun ref sid q []

-- | Replace every indexed parameter with the corresponding provided parameter.
--   Keep all non-indexed parameters in place.
replaceParams :: [Either Int Param] -> [Param] -> [Param]
replaceParams params = map fromRight . go firstParamIx params
  where
    go n ps (x:xs) = go (n+1) (map (subst n x) ps) xs
    go _ ps _      = ps

    subst n x (Left n') | n == n' = Right x
    subst _ _ old                 = old

    fromRight (Right x) = x
    fromRight _         = error "BUG: query parameter not substituted!"

-- | Inspect a list of parameters, denoting each parameter with either a
--   placeholder index or a literal parameter.
inspectParams :: [SqlTypeRep] -> [Param] -> IO ([Either Int Param], [SqlTypeRep])
inspectParams ts (x:xs) = do
  res <- try $ pure $! forceParam x
  let (x', t) = case res of
        Right p               -> (Right p, paramType p)
        Left (Placeholder ix) -> (Left ix, ts !! ix)
  (xs', ts') <- inspectParams ts xs
  return (x' : xs', t : ts')
inspectParams _ [] = do
  return ([], [])

-- | Force a parameter deep enough to determine whether it is a placeholder.
forceParam :: Param -> Param
forceParam p@(Param (LCustom _ x)) | x `seq` True = p
forceParam p                                      = p
