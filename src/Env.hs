module Env where

import Data.Pool
import Database.Persist.Sqlite

-- ^. is also in esqulito, not always a lense
-- haskell lens ^. view, .~ set, over %~, ?~ set maybe values, +~ add value to current value
-- hard to think about data as functions
-- pattern matching inside cse statement, can you pattern match anywhere, or have to do second case
-- case vs pattern matching
data Env = Env { envPool :: Pool SqlBackend }