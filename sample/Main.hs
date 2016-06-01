{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Database.Tansu
import Database.Tansu.Backend.SQLite3

main :: IO ()
main = void $ withSQLiteDb "sample.db" $ \ db -> do
  putStrLn "Populating test database"
  run db $ do
    "one"   =: "un"
    "two"   =: "du"
    "three" =: "tri"
    "four"  =: "kvar"

  putStr "looking up key 'three': "
  rs <- run db $ get "three"
  case rs of
    Right val -> print val
    Left _    -> putStrLn "...not in the database."

  putStr "looking up key 'five': "
  rs <- run db $ get "five"
  case rs of
    Right val -> print val
    Left _    -> putStrLn "...not in the database."
