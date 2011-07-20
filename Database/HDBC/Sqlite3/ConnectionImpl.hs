module Database.HDBC.Sqlite3.ConnectionImpl where

import qualified Database.HDBC.Statement as Types
import qualified Database.HDBC.Types as Types
import Database.HDBC.ColTypes as ColTypes
import Foreign.C.Types

data Connection = 
    Connection {
                disconnect :: IO (),
                commit :: IO (),
                rollback :: IO (),
                run :: String -> [Types.SqlValue] -> IO Integer,
                runRaw :: String -> IO (),
                prepare :: String -> IO Types.Statement,
                clone :: IO Connection,
                hdbcDriverName :: String,
                hdbcClientVer :: String,
                proxiedClientName :: String,
                proxiedClientVer :: String,
                dbServerVer :: String,
                dbTransactionSupport :: Bool,
                getTables :: IO [String],
                describeTable :: String -> IO [(String, ColTypes.SqlColDesc)],
                -- | Sets the timeout for a lock before returning a busy error.
                -- Give the time in milliseconds.
                setBusyTimeout :: CInt -> IO ()
               }

instance Types.IConnection Connection where
  disconnect = disconnect
  commit = commit
  rollback = rollback
  run = run
  runRaw = runRaw
  prepare = prepare
  clone = clone
  hdbcDriverName = hdbcDriverName
  hdbcClientVer = hdbcClientVer
  proxiedClientName = proxiedClientName
  proxiedClientVer = proxiedClientVer
  dbServerVer = dbServerVer
  dbTransactionSupport = dbTransactionSupport
  getTables = getTables
  describeTable = describeTable
