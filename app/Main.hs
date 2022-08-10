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

getLineItemNames :: MonadIO m => SqlPersistT m [Entity LineItem]
-- getLineItemNames :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
getLineItemNames = select $ from $ table @LineItem
  -- liftIO $ mapM_ (putStrLn . lineItemName . entityVal) items

-- getLineItemNames:: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
-- getLineItemNames = do
--   items <- selectList [lineItemName]

runApp :: AppT ()
runApp = do
  total <- runDB $ do -- do inside a do block ? same monad ?
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    getLineItemTotal
  names <- runDB $ do getLineItemNames
  liftIO . putStrLn $ intercalate " " $ fmap (lineItemName . entityVal) names
  let remainingBudget = weeklyBudget - total
  liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget


processUserInput :: String -> IO ()
processUserInput input = case parseUserInput input of
  "c" -> liftIO .  putStrLn $ "you said command c"
  "p" -> liftIO .  putStrLn $ "you said command p"
  otherwise -> liftIO .  putStrLn $ "invalid command"
  where parseUserInput input = splitOn " " input !! 0

-- commandC :: [String] -> IO()


interact :: IO ()
interact = do
  putStrLn $ "Hello,\nwaiting for input:" 
  forever $ do
    input <- getLine
    processUserInput input
    putStr $ "waiting for input: "
    hFlush stdout

main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT (runDB (runMigration migrateAll)) env
  runAppT runApp env
  Main.interact