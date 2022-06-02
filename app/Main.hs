{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-

Tool chain installation:

  https://www.haskell.org/ghcup/steps/

Run program:

    echo app/Main.hs | entr -c cabal run pg-haskell

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
module Main (
    main,
) where

import Control.Monad (
    unless,
    void,
 )
import DBConfig (dbConn)
import Data.Int (Int32)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Typed as PG (
    PGConnection,
    pgConnect,
    pgExecute,
    pgQuery,
    pgSQL,
 )

data Client = Client Int32 String
    deriving (Eq)

insertClient :: PGConnection -> Client -> IO ()
insertClient pg (Client cid name) =
    void $
        pgExecute
            pg
            [pgSQL|INSERT INTO clients (id, name) VALUES (${cid}, ${name})|]

getClient :: PGConnection -> Int32 -> IO (Maybe Client)
getClient pg cid =
    fmap (uncurry Client) . listToMaybe
        <$> pgQuery
            pg
            [pgSQL|SELECT id, name FROM clients WHERE id = ${cid}|]

main :: IO ()
main = do
    db <- pgConnect =<< dbConn
    let john = Client 1 "John Doe"
    insertClient db john
    johnAgain <- getClient db 1
    unless (johnAgain == Just john) $ fail "Clients don't match!"
    putStrLn "DONE!"
