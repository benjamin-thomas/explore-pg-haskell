{-

Tool chain installation:

  https://www.haskell.org/ghcup/steps/

Run program:

    echo app/Main.hs | entr -c cabal run pg-haskell
    echo app/Main.hs | entr -c runghc ./app/Main.hs
    echo app/Main.hs | entr -c runhaskell ./app/Main.hs

Build program:

    cabal build

Run REPL:

    ghci
    cabal repl

Show the type signature in the REPL

    > import System.Environment
    > :t getEnv
    > :t lookupEnv

Learning:

    https://www.haskell.org/ghcup/steps/#how-to-learn-haskell-proper

        - https://github.com/haskell-beginners-2022/course-plan
        - https://www.cis.upenn.edu/~cis194/spring13

References:

    - https://riptutorial.com/haskell/example/15526/postgres
    - https://lotz84.github.io/haskellbyexample

-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Following this tutorial: https://tuttlem.github.io/2020/10/30/postgresql-data-access-with-haskell.html

import Data.Int (Int64)
import Database.PostgreSQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectUser
      ),
    Connection,
    FromRow,
    Only (Only),
    connect,
    defaultConnectInfo,
    query,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import GHC.GHCi.Helpers (flushAll)

-- import System.Environment

-- x :: String
-- x = "YOZ"

-- home :: IO String
-- home = getEnv "HOME"

-- mHome :: IO (Maybe String)
-- mHome = lookupEnv "HOME"

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "SHOULD_LOAD_FROM_ENV",
      connectDatabase = "SHOULD_LOAD_FROM_ENV",
      connectUser = "SHOULD_LOAD_FROM_ENV",
      connectPassword = "SHOULD_LOAD_FROM_ENV"
    }

data Client = Client {id :: Int, name :: String}
  deriving (Show)

instance FromRow Client where
  fromRow = Client <$> field <*> field

selectClient :: Connection -> Int -> IO [Client]
selectClient conn cid = query conn "SELECT id, name FROM clients WHERE id = ?" $ Only cid

insertClient :: Connection -> String -> IO [Only Int64]
insertClient conn name_ =
  query conn "INSERT INTO clients (name) VALUES (?) RETURNING id" $ Only name_

main :: IO ()
main = do
  conn <- connect localPG
  putStr "What is the client's name? "
  flushAll
  clientName <- getLine
  cid <- insertClient conn clientName
  putStrLn $ "New Client: " ++ show cid
  putStrLn "Now retrieving client #1031600975 from test database..."
  client <- selectClient conn 1031600975
  putStrLn $ "Client #1031600975 details: " ++ show client
  putStrLn "Finished!"

--   mapM_ print =<< (query_ conn "SELECT 1 + 2" :: IO [Only Int])
