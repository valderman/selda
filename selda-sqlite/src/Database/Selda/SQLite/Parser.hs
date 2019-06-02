{-# LANGUAGE OverloadedStrings #-}
-- | Incomplete parser for SQL CREATE TABLE statements.
--   Needed to figure out whether any given column is auto-incrementing
--   or not. It's super inefficient, but doesn't really matter since it'll
--   only ever be invoked during validation.
module Database.Selda.SQLite.Parser (colsFromQuery) where
import Control.Applicative
import Control.Monad (void, msum, MonadPlus (..))
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Maybe (isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

colsFromQuery :: Text -> [(Text, (Text, Bool))]
colsFromQuery = parse' parseCreateQueryCols

newtype Parser a = P { unP :: (Text -> Maybe (Text, a)) }

instance Functor Parser where
  fmap f (P g) = P (fmap (fmap f) . g)

instance Applicative Parser where
  pure x = P $ \t -> Just (t, x)
  f <*> x = f >>= \f' -> fmap f' x

instance Alternative Parser where
  empty = P $ const Nothing
  P f <|> P g = P $ \s ->
    case f s of
      res@(Just _) -> res
      _            -> g s

instance Monad Parser where
  return = pure
  P m >>= f = P $ \s -> do
    case m s of
      Just (rest, x) -> unP (f x) rest
      _              -> Nothing

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parse :: Parser a -> Text -> Maybe a
parse (P f) t = snd <$> f t

parse' :: Parser a -> Text -> a
parse' f t = maybe (error $ "no parse: '" ++ show t ++ "'") id $ parse f t

lowerText :: Text -> Parser ()
lowerText prefix = P $ \s ->
  case Text.splitAt (Text.length prefix) s of
    (prefix', rest) | prefix == Text.toLower prefix' -> Just (rest, ())
    _                                                -> Nothing

charP :: (Char -> Bool) -> Parser Char
charP p = P $ \s ->
  case Text.splitAt 1 s of
    (prefix, rest) | Text.any p prefix -> Just (rest, Text.head prefix)
    _                                  -> Nothing

char :: Char -> Parser Char
char c = charP (== c)

space :: Parser ()
space = void $ charP isSpace

spaces :: Parser ()
spaces = void $ some space

sepBy1 :: Parser s -> Parser a -> Parser [a]
sepBy1 sep p = do
  x <- p
  xs <- optional $ sep *> sepBy1 sep p
  case xs of
    Just xs' -> pure (x:xs')
    _        -> pure [x]

commaSeparated :: Parser a -> Parser [a]
commaSeparated = sepBy1 (many space >> char ',' >> many space)

keywords :: [Text]
keywords = ["constraint", "unique", "primary key"]

parseCreateQueryCols :: Parser [(Text, (Text, Bool))]
parseCreateQueryCols = do
  lowerText "create table"
  spaces
  void $ sqliteIdentifier
  void $ many space
  void $ char '('
  cols <- commaSeparated parseCol <* many space
  void $ char ')'
  pure $ catMaybes cols

parseCol :: Parser (Maybe (Text, (Text, Bool)))
parseCol = do
    decl <- constraint <|> column
    pure $ case decl of
      Right col -> Just col
      _         -> Nothing
  where
    column = do
      name <- sqliteIdentifier
      spaces
      ty <- sqliteIdentifier
      void $ optional $ spaces *> lowerText "primary key"
      isAuto <- optional $ spaces *> lowerText "autoincrement"
      void $ many $ charP (\c -> isAlphaNum c || isSpace c)
      void $ optional $ do
        void $ char '('
        void $ commaSeparated sqliteIdentifier
        void $ char ')'
      pure $ Right $ (name, (ty, isJust isAuto))
    constraint = do
      msum (map lowerText keywords)
      void $ many $ msum
        [ void sqliteIdentifier
        , void $ do
            void $ char '('
            void $ commaSeparated sqliteIdentifier
            void $ char ')'
        , spaces
        ]
      pure $ Left ()

sqliteIdentifier :: Parser Text
sqliteIdentifier = Text.pack <$> (quoted <|> unquoted)
  where
    unquoted = do
      x <- charP $ \c -> isAlpha c || c == '_'
      xs <- many $ charP $ \c -> isAlphaNum c || c == '_' || c == '$'
      pure $ (x:xs)
    quoted = char '"' *> many quotedChar <* char '"'
    quotedChar = (char '"' >> char '"') <|> charP (/= '"')
