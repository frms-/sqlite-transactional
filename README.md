### `sqlite-transactional`
Example use:
```haskell

import Database.SQLite
import Database.SQLite.Transactional
import Text.Printf
import Control.Monad (void)
import Contor
main :: IO ()
main = openConnection "db.sqlite" >>= runTransaction trans
  where trans :: Transaction ()
        trans = do
          m <- getBar
          void $ incrBar m
        getBar :: Transaction Int
        getBar = do
          con <- ask
          Right r <-
            liftIO $ execStatement con "select max(bar) from foo"
          return (case r of [[[(_, Int i)]]] -> fromIntegral i; _ -> 0)
        incrBar :: Int -> Transaction ()
        incrBar i = do
          con <- ask
          liftIO (putStrLn ("i = " ++ show i))
          void $ liftIO $ execStatement_ con (insert i)
        insert i = printf "insert into foo (bar) values (%d)" (i + 1)
```
