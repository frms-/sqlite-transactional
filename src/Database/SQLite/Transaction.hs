{-# LANGUAGE General0izedNewtypeDeriving #-}

-- |
-- Module:      Database.SQLite.Transaction
-- Copyright:   (c) 2016 Fredrik Malmros.
-- License:     MIT
-- Stability:   experimental
-- Portability: GHC

module Database.SQLite.Transaction ( runTransaction
                                   , runTransaction'
                                   , Transaction(..)
                                   , TransactionType(..)
                                   ) where

import Database.SQLite
import Control.Monad.Reader
import Control.Exception

-- | The SQLite transaction monad transformer.
newtype Transaction a = MkTransaction (ReaderT SQLiteHandle IO a )
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader SQLiteHandle
           , MonadIO)

-- | The SQLite transaction type. See <https://www.sqlite.org/lang_transaction.html>.
data TransactionType = Deferred | Immediate | Exclusive

instance Show TransactionType where
  show Deferred  = "DEFERRED"
  show Immediate = "IMMEDIATE"
  show Exclusive = "EXCLUSIVE"

-- | Run a deferred transaction.
runTransaction ::  Transaction a -> SQLiteHandle -> IO a
runTransaction trans con = runTransaction' Deferred trans con

-- | Run a transaction of the given transaction type.
runTransaction' :: TransactionType -> Transaction a -> SQLiteHandle -> IO a
runTransaction' typ (MkTransaction tr) con =
  mask $ \restore -> do
  begin con typ
  res <- try $ restore (runReaderT tr con)
  case res of
    Left ex -> rollback con >> throwIO (ex ::SomeException)
    Right a -> do
      commit con
      return a

begin :: SQLiteHandle -> TransactionType -> IO ()
begin con ty = void $ execStatement_ con ("BEGIN TRANSACTION " ++ show ty)

commit :: SQLiteHandle -> IO ()
commit con =  void $ execStatement_ con "END TRANSACTION"

rollback :: SQLiteHandle -> IO ()
rollback con = void $ execStatement_ con "ROLLBACK"
