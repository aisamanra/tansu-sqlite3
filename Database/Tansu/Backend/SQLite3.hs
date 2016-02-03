{-# LANGUAGE OverloadedStrings #-}

module Database.Tansu.Backend.SQLite3 where

import           Control.Monad (when, void)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite3
import           Database.Tansu.Internal
import           System.Directory (doesFileExist)

schema :: Text
schema = "CREATE TABLE tansu ( k blob primary key not null \
                           \ , v blob not null );"

sqlset :: Database -> ByteString -> ByteString -> IO (Either TansuError ())
sqlset db k v = do
  stmt <- prepare db "INSERT INTO tansu (k, v) VALUES (:k, :v);"
  bindNamed stmt [ (":k", SQLBlob k)
                 , (":v", SQLBlob v)
                 ]
  void (step stmt)
  finalize stmt
  return (return ())

sqlget :: Database -> ByteString -> IO (Either TansuError ByteString)
sqlget db k = do
  stmt <- prepare db "SELECT v FROM tansu WHERE k = :k;"
  bindNamed stmt [ (":k", SQLBlob k) ]
  void (step stmt)
  cs <- columns stmt
  finalize stmt
  case cs of
    [SQLBlob v] -> return (Right v)
    [SQLNull]   -> return (Left (KeyNotFound k))
    rs          -> return (Left (OtherError (show rs)))

sqldel :: Database -> ByteString -> IO (Either TansuError ())
sqldel db k = do
  stmt <- prepare db "DELETE FROM tansu WHERE k = :k;"
  bindNamed stmt [ (":k", SQLBlob k) ]
  void (step stmt)
  finalize stmt
  return (Right ())

sqltransact :: Database -> IO a -> IO a
sqltransact db mote = do
  exec db "BEGIN TRANSACTION;"
  result <- mote
  exec db "END TRANSACTION;"
  return result

withSQLiteDb :: FilePath -> (TansuDb k v -> IO a) -> IO a
withSQLiteDb path mote = do
  exists <- doesFileExist path
  conn <- open (T.pack path)
  when (not exists) $ do
    exec conn schema
  result <- mote $ TansuDb { dbGet = sqlget conn
                           , dbSet = sqlset conn
                           , dbDel = sqldel conn
                           , dbRunTransaction = id
                           }
  close conn
  return result
