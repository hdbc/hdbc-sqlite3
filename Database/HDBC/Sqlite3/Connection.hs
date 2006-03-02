{-# CFILES hdbc-sqlite3-helper.c #-}
-- above line for hugs
{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

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

module Database.HDBC.Sqlite3.Connection where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
import Database.HDBC.Sqlite3.Types
import Database.HDBC.Sqlite3.Statement
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Database.HDBC.Sqlite3.Utils
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar

{- | Connect to an Sqlite version 3 database.  The only parameter needed is
the filename of the database to connect to.

All database accessor functions are provided in the main HDBC module. -}
connectSqlite3 :: FilePath -> IO Connection
connectSqlite3 fp = 
    withCString fp 
        (\cs -> alloca 
         (\(p::Ptr (Ptr CSqlite3)) ->
              do res <- sqlite3_open cs p
                 o <- peek p
                 fptr <- newForeignPtr sqlite3_closeptr o
                 newconn <- mkConn fp fptr
                 checkError ("connectSqlite3 " ++ fp) fptr res
                 return newconn
         )
        )

mkConn :: FilePath -> Sqlite3 -> IO Connection
mkConn fp obj =
    do children <- newMVar []
       begin_transaction obj children
       ver <- (sqlite3_libversion >>= peekCString)
       return $ Connection {
                            disconnect = fdisconnect obj children,
                            commit = fcommit obj children,
                            rollback = frollback obj children,
                            run = frun obj children,
                            prepare = newSth obj children,
                            clone = connectSqlite3 fp,
                            hdbcDriverName = "sqlite3",
                            hdbcClientVer = ver,
                            proxiedClientName = "sqlite3",
                            proxiedClientVer = ver,
                            dbServerVer = ver,
                            getTables = fgettables obj children}

fgettables o mchildren =
    do sth <- newSth o mchildren "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
       execute sth []
       res1 <- fetchAllRows sth
       let res = map fromSql $ concat res1
       return $ seq (length res) res

--------------------------------------------------
-- Guts here
--------------------------------------------------

begin_transaction :: Sqlite3 -> ChildList -> IO ()
begin_transaction o children = frun o children "BEGIN" [] >> return ()

frun o mchildren query args =
    do sth <- newSth o mchildren query
       res <- execute sth args
       finish sth
       return res

fcommit o children = do frun o children "COMMIT" []
                        begin_transaction o children
frollback o children = do frun o children "ROLLBACK" []
                          begin_transaction o children

fdisconnect :: Sqlite3 -> ChildList -> IO ()
fdisconnect o mchildren = withRawSqlite3 o $ \p -> 
    do closeAllChildren mchildren
       r <- sqlite3_close p
       checkError "disconnect" o r

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_open2"
  sqlite3_open :: CString -> (Ptr (Ptr CSqlite3)) -> IO CInt

foreign import ccall unsafe "hdbc-sqlite3-helper.h &sqlite3_close_finalizer"
  sqlite3_closeptr :: FunPtr ((Ptr CSqlite3) -> IO ())

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_close_app"
  sqlite3_close :: Ptr CSqlite3 -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_libversion"
  sqlite3_libversion :: IO CString