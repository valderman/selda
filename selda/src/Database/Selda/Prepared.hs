{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
-- | Building and executing prepared statements.
module Database.Selda.Prepared (Preparable, Prepare, prepared) where
import Database.Selda.Backend.Internal
import Database.Selda.Column
import Database.Selda.Compile
import Database.Selda.Query.Type
import Database.Selda.SQL (param, paramType)
import Database.Selda.Types (TableName)
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Proxy
import Data.Text (Text)
import System.IO.Unsafe

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
  Equiv (Query s a)    (m [b])  = Res a ~ b

class Preparable q where
  -- | Prepare the query and parameter list.
  mkQuery :: MonadSelda m
          => Int -- ^ Next argument index.
          -> q   -- ^ The query.
          -> [SqlTypeRep] -- ^ The list of param types so far.
          -> m (Text, [Either Int Param], [SqlTypeRep], [TableName])

-- | Some parameterized query @q@ that can be prepared into a function @f@
--   in some @MonadSelda@.
class Prepare q f where
  -- | Build the function that prepares and execute the query.
  mkFun :: Preparable q => StmtID -> q -> [Param] -> f

instance (SqlType a, Prepare q b) => Prepare q (a -> b) where
  mkFun sid qry ps x = mkFun sid qry (param x : ps)

instance (MonadSelda m, a ~ Res (ResultT q), Result (ResultT q)) =>
         Prepare q (m [a]) where
  mkFun sid qry arguments = do
    conn <- seldaConnection
    let backend = connBackend conn
        args = reverse arguments
    stmts <- liftIO $ readIORef (connStmts conn)
    case M.lookup sid stmts of
      Just stm -> do
        -- Statement already prepared for this connection; just execute it.
        liftIO $ do
          let hdl = stmtHandle stm
          res <- runPrepared backend hdl (replaceParams (stmtParams stm) args)
          return $ map (toRes (Proxy :: Proxy (ResultT q))) (snd res)
      _ -> do
        -- Statement wasn't prepared for this connection; prepare and execute.
        (q, params, reps, ts) <- mkQuery firstParamIx qry []
        liftIO $ do
          hdl <- prepareStmt backend sid reps q
          let params' = replaceParams params args
              stm = SeldaStmt
                { stmtHandle = hdl
                , stmtParams = params
                , stmtTables = ts
                }
          atomicModifyIORef' (connStmts conn) $ \m -> (M.insert sid stm m, ())
          res <- runPrepared backend hdl params'
          return $ map (toRes (Proxy :: Proxy (ResultT q))) (snd res)

instance (SqlType a, Preparable b) => Preparable (Col s a -> b) where
  mkQuery n f ts = mkQuery (n+1) (f x) (sqlType (Proxy :: Proxy a) : ts)
    where x = C $ Lit $ LCustom (throw (Placeholder n) :: Lit a)

instance Result a => Preparable (Query s a) where
  mkQuery _ q types = do
    b <- seldaBackend
    case compileWithTables (ppConfig b) q of
      (tables, (q', ps)) -> do
        (ps', types') <- liftIO $ inspectParams (reverse types) ps
        return (q', ps', types', tables)

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
--   queries aren't re-prepared a single time more than absolutely necessary,
--   consider adding a @NOINLINE@ annotation to each prepared function.
--
--   A usage example:
--
-- > ages :: Table (Text :*: Int)
-- > ages = table "ages" $ primary "name" :*: required "age"
-- >
-- > {-# NOINLINE ageOf #-}
-- > ageOf :: Text -> SeldaM [Int]
-- > ageOf = prepared $ \n -> do
-- >   (name :*: age) <- select ages
-- >   restrict $ name .== n
-- >   return age
{-# NOINLINE prepared #-}
prepared :: (Preparable q, Prepare q f, Equiv q f) => q -> f
prepared q = unsafePerformIO $ do
  sid <- freshStmtId
  return $ mkFun sid q []

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
forceParam p@(Param (LCustom x)) | x `seq` True = p
forceParam p                                    = p
