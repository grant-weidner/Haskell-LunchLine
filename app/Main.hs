{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import AppT
import Env
import Parser
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad (forever)
import Database.Persist.Sqlite hiding ((=.),(<.),(>.), update)
import Database.Persist.TH
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, Maybe(..))
import Database.Esqueleto.Experimental
import System.IO
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Int
  created UTCTime default=CURRENT_TIME
  deriving Show
Budget
  amount Int
  deriving Show
|]

runDB :: ReaderT SqlBackend IO a -> AppT a
runDB body = do
  pool <- asks envPool
  liftIO $ runSqlPool body pool
  
weeklyBudget :: Int
weeklyBudget = 100

spend :: Int -> LineItem -> Int
spend n LineItem{..} = n - (lineItemAmount)

getLineItemTotal :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
getLineItemTotal = selectSum $ do
  items <- from $ table @LineItem
  pure $ sum_ $ items ^. LineItemAmount
 where
  selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne

-- getLineItemTotalLastDay :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
-- getLineItemTotalLastDay = select $ do
--   lineItems <- from $ table @LineItem
--     where_ $
--       lineItems ^. LineItemCreated <. val time24HoursAgo
--   pure $ sum_ $ lineItems ^. LineItemAmount
--  where
--   time24HoursAgo = liftIO $ fmap (addUTCTime (-nominalDay)) getCurrentTime
--   selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne

getLineItemTotalSince:: MonadIO m => UTCTime -> SqlPersistT m [Entity LineItem]
getLineItemTotalSince t = 
  select $ do
    items <- from $ table @LineItem
    where_ $ items ^. LineItemCreated >. val t 
    -- liftIO $ putStrLn $ show items
    pure items

testGetLineItemTotalSince:: MonadIO m => SqlPersistT m [Entity LineItem]
testGetLineItemTotalSince = do
  time24HoursAgo <- liftIO $ fmap (addUTCTime (-nominalDay)) getCurrentTime
  getLineItemTotalSince time24HoursAgo 

showLastDayStats :: AppT ()
showLastDayStats = do 
  time24HoursAgo <- liftIO $ fmap (addUTCTime (-nominalDay)) getCurrentTime
  lastDayItems <- runDB $ getLineItemTotalSince time24HoursAgo 
  numberLastDayItems <- pure $ length lastDayItems
  totalSpentLastDay <- pure $ sum $ fmap (lineItemAmount . entityVal) lastDayItems
  liftIO $ putStrLn $ "Last Day Stats:"
  liftIO $ putStrLn $ "  total spent in the last day: " <> show totalSpentLastDay
  liftIO $ putStrLn $ "  number of items bought in the last day: " <> show numberLastDayItems
  liftIO $ putStrLn $ "  average cost of item in the last day: " <> show (totalSpentLastDay `div` numberLastDayItems)
  
getLineItems :: MonadIO m => SqlPersistT m [Entity LineItem]
getLineItems = select $ from $ table @LineItem

getLineItemNames :: AppT [String]
getLineItemNames = do
  items <- runDB $ do getLineItems
  pure $ fmap (lineItemName . entityVal) items

getLineItemsInShowFormat :: AppT [(String, Int)]
getLineItemsInShowFormat = do 
  items <- runDB $ getLineItems
  pure $ fmap formatLineItem items where
    formatLineItem s = (lineItemNameEV s, lineItemAmountEV s) where
      lineItemNameEV = lineItemName . entityVal
      lineItemAmountEV = lineItemAmount . entityVal

showLineItems :: AppT ()
showLineItems = do
  itemsToShow <- getLineItemsInShowFormat
  liftIO . putStrLn $ intercalate "\n"  (fmap show itemsToShow)

addItem :: String -> Int -> AppT ()
addItem name amount = do 
  runDB $ do 
    time <- liftIO getCurrentTime
    insert_ $ LineItem name amount time
  liftIO $ putStrLn $ "added " <> name <> ": " <> show amount <> " to the DB"

updateBudget :: Int -> AppT ()
updateBudget a = do 
  runDB $ update $ \b -> do
    set b [BudgetAmount =. val a]
  liftIO $ putStrLn $ "updated budget to: " <> show a
  
getBudget :: AppT Int
getBudget = do 
  budget <- runDB $ select $ from $ table @Budget 
  pure $  (head (fmap (budgetAmount . entityVal) budget) )

showBudget :: AppT ()
showBudget = do
  budget <- getBudget
  liftIO $ putStrLn $ "budget is: " ++ show budget

showRemainingBudget :: AppT ()
showRemainingBudget = do
  totalSpent <- runDB $ getLineItemTotal
  budget <- getBudget
  let remaining = budget - totalSpent
  liftIO $ putStrLn $ "remaining budget is: " <> (show remaining)

getTotalSpent :: AppT ()
getTotalSpent = do
  total <- runDB $ getLineItemTotal @Int
  liftIO $ putStrLn $ "total spent so far is: " <> show total 

-- processUserInput :: String -> AppT ()
-- processUserInput input = case parseUserInput input of
--   "c" -> liftIO .  putStrLn $ "you said command c" 
--   "p" -> liftIO .  putStrLn $ "you said command p"
--   otherwise -> liftIO .  putStrLn $ "invalid command"
--   where parseUserInput input = splitOn " " input !! 0

interact :: AppT ()
interact = do
  liftIO . putStrLn $ "Hello,\nwaiting for input:" 
  forever $ do
    input <- liftIO $ getLine
    let result = parse (parseC <|> parseP <|> parseT <|> parseU <|> parseS <|> parseR <|> parseD) input -- does this take you out of monadic context
    case result of
      -- Right ('c', Just name, Just amount) -> putStrLn "success! command c"
      Right ('c', Just name, Just amount) -> addItem name amount
      Right ('p', Nothing, Nothing) -> showLineItems
      Right ('t', Nothing, Nothing) -> getTotalSpent
      Right ('u', Nothing, Just amount) -> updateBudget amount
      Right ('s', Nothing, Nothing) -> showBudget
      Right ('r', Nothing, Nothing) -> showRemainingBudget
      Right ('d', Nothing, Nothing) -> showLastDayStats
      Left _ -> liftIO $ putStrLn "whoops, error"
      _ -> liftIO $ putStrLn "case not caught"
    liftIO $ putStrLn $ "waiting for input: "
    liftIO $ hFlush stdout

runApp :: AppT ()
runApp = do
  addItem "Pizza" 11
  addItem "Burger" 12
  runDB $ insert_ $ Budget 100
  total <- runDB $ getLineItemTotal
  names <- runDB $ getLineItems
  liftIO . putStrLn $ intercalate " " $ fmap (lineItemName . entityVal) names
  let remainingBudget = weeklyBudget - total
  liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget
  Main.interact
  
main :: IO ()
main = do
  env <- runNoLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT (runDB (runMigration migrateAll)) env
  runAppT runApp env