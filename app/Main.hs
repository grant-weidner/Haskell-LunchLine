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
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental
import System.IO

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
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

processUserInput :: String -> AppT ()
processUserInput input = case parseUserInput input of
  "c" -> liftIO .  putStrLn $ "you said command c"
  "p" -> liftIO .  putStrLn $ "you said command p"
  otherwise -> liftIO .  putStrLn $ "invalid command"
  where parseUserInput input = splitOn " " input !! 0

interact :: AppT ()
interact = do
  liftIO . putStrLn $ "Hello,\nwaiting for input:" 
  liftIO . forever $ do
    input <- getLine
    let result = parse (parseC <|> parseP) input -- does this take you out of monadic context
    case result of
      Right ('c', Just name, Just amount) -> putStrLn "success! command c"
      Right ('p', Nothing, Nothing) -> putStrLn "success! command p"
      Left _ -> putStrLn "whoops, error"
      otherwise -> putStrLn "case not caught"
    putStr $ "waiting for input: "
    hFlush stdout

runApp :: AppT ()
runApp = do
  total <- runDB $ do
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    getLineItemTotal
  names <- runDB $ do getLineItems
  liftIO . putStrLn $ intercalate " " $ fmap (lineItemName . entityVal) names
  let remainingBudget = weeklyBudget - total
  liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget
  Main.interact
  
main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT (runDB (runMigration migrateAll)) env
  runAppT runApp env