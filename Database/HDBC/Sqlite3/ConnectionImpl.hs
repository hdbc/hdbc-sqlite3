{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}
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
