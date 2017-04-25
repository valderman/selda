{-# LANGUAGE TypeOperators, OverloadedStrings, DeriveGeneric #-}
import Web.Scotty
import Database.Selda hiding (text)
import Database.Selda.SQLite
import Database.Selda.Generic
import Data.Aeson (ToJSON)
import qualified Data.Text.Lazy as Lazy (Text)

dbFile :: FilePath
dbFile = "shoppinglist.sqlite"

data Row = Row
  { rowItem  :: Text
  , rowCount :: Int
  , rowTotal :: Double
  } deriving Generic
instance ToJSON Row

data List = List
  { listRows :: [Row]
  , listCost :: Double
  } deriving Generic
instance ToJSON List

-- | Run a computation over the application's database.
--   If the database doesn't exist, it gets created.
withDB :: SeldaT IO a -> ActionM a
withDB act = liftIO $ withSQLite dbFile $ do
  tryCreateTable items
  act

-- | Declare the @items@ table and its selectors.
items :: Table (Text :*: Double)
(items, itemName :*: price)
  = tableWithSelectors "list"
  $   required "item"
  :*: required "price"

-- | Add an item to the shopping list.
addItem :: MonadSelda m => Text -> Double -> m ()
addItem name price = insert_ items [name :*: price]

-- | Get the total cost of all items on the shopping list.
getCost :: MonadSelda m => m Double
getCost = fmap head $ query $ aggregate $ do
  item <- select items
  return $ sum_ (item ! price)

-- | Get all items in the shopping list, with duplicates coalesced into a single
--   row with the item name, the number of items, and the total cost
--   of the items.
getList :: MonadSelda m => m List
getList = do
  rows <- query $ aggregate $ do
    item <- select items
    name <- groupBy (item ! itemName)
    return (name :*: count (item ! price) :*: sum_ (item ! price))
  total <- getCost
  return (List (fromRels rows) total)

-- | Remove all items from the list.
clearList :: MonadSelda m => m ()
clearList = deleteFrom_ items (const true)

-- | Helpfully list all endpoints.
helpMessage :: Lazy.Text
helpMessage = mconcat
  [ "endpoints:\n"
  , "  /list             - get the entire shopping list\n"
  , "  /clear            - remove all items from the list\n"
  , "  /cost             - get the total cost of all items\n"
  , "  /add/:item/:price - add a new item named :item, costing :price\n"
  ]

-- | Expose the application as a JSON-based API.
main :: IO ()
main = scotty 8000 $ do
  get "/"      $ text helpMessage
  get "/list"  $ withDB getList >>= json
  get "/clear" $ withDB clearList >>= json
  get "/cost" $ withDB getCost >>= json
  get "/add/:item/:price" $ do
    item <- param "item"
    price <- param "price"
    withDB $ addItem item price
