{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.SQLite.Transaction ( runTransaction
                                   , Transaction(..)
                                   ) where

import Database.SQLite
import Control.Monad.Reader
import Control.Exception

newtype Transaction a = Transaction (ReaderT SQLiteHandle IO a )
             deriving
               ( Functor
               , Applicative
               , Monad
               , MonadReader SQLiteHandle
               , MonadIO)


runTransaction ::  Transaction a -> SQLiteHandle -> IO a
runTransaction (Transaction t) con =
  mask $ \restore -> do
  begin con
  res <- try $ restore (runReaderT t con)
  case res of
    Left ex -> rollback con >> throwIO (ex ::SomeException)
    Right a -> do
      commit con
      return a

begin :: SQLiteHandle -> IO ()
begin con = void $ execStatement_ con "BEGIN TRANSACTION"

commit :: SQLiteHandle -> IO ()
commit con =  void $ execStatement_ con "END TRANSACTION"

rollback :: SQLiteHandle -> IO ()
rollback con = void $ execStatement_ con "ROLLBACK"
