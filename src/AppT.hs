{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppT where


import Env
import Control.Monad.Reader

newtype AppT a = AppT { unAppT :: ReaderT Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runAppT :: MonadIO m => AppT a -> Env -> m a
runAppT body env = liftIO $ runReaderT (unAppT body) env