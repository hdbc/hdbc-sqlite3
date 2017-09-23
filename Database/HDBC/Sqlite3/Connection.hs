{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# CFILES hdbc-sqlite3-helper.c #-}
-- above line for hugs

module Database.HDBC.Sqlite3.Connection
  ( connectSqlite3
  , connectSqlite3Raw
  , connectSqlite3Ext
  , Impl.Connection()
  )
  where

import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.DriverUtils
import qualified Database.HDBC.Sqlite3.ConnectionImpl as Impl
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
import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.Char

{- | Connect to an Sqlite version 3 database.  The only parameter needed is
the filename of the database to connect to.

All database accessor functions are provided in the main HDBC module. -}
connectSqlite3 :: FilePath -> IO Impl.Connection
connectSqlite3 = connectSqlite3Ext True False

{- | Connects to a Sqlite v3 database as with 'connectSqlite3', but
instead of converting the supplied 'FilePath' to a C String by performing
a conversion to Unicode, instead converts it by simply dropping all bits past
the eighth.  This may be useful in rare situations
if your application or filesystemare not running in Unicode space. -}
connectSqlite3Raw :: FilePath -> IO Impl.Connection
connectSqlite3Raw = connectSqlite3Ext True True

{- | Connect to an Sqlite version 3 database as with connectSqlite3, but if
auto-finish is disabled, HDBC will not auto-finish prepared statements after
the last row is fetched. Keeping the statement in its prepared state improves
the performance of repeated execution of cached prepared statements, and
eliminates the overhead of tracking open statement handles by HDBC.

With auto-finish disabled, the application is responsible for explicitly
finishing all application prepared statements before @disconnect@ is called.
Otherwise, the SQLite3 database may, at that time, throw an exception when
some prepared statements are still open, they may not be finalized in time
via garbage collection even if they are already out of scope.

The filesystem in which the database resides is by default assumed to support
UTF-8 filenames.  If that's not the case, set @raw@ to 'True' and provide a
'FilePath` that holds the byte encoding of the native filename. -}
connectSqlite3Ext :: Bool     -- ^ Auto-finish statements
                  -> Bool     -- ^ If true Raw 8-bit name encoding else UTF-8
                  -> FilePath -- ^ Database file name
                  -> IO Impl.Connection
connectSqlite3Ext auto raw =
     let nameDecoder = if raw then withCString
                              else (B.useAsCString . BUTF8.fromString)
     in genericConnect nameDecoder auto raw

genericConnect :: (String -> (CString -> IO Impl.Connection) -> IO Impl.Connection)
               -> Bool
               -> Bool
               -> FilePath
               -> IO Impl.Connection
genericConnect strAsCStrFunc auto raw fp =
    strAsCStrFunc fp
        (\cs -> alloca
         (\(p::Ptr (Ptr CSqlite3)) ->
              do res <- sqlite3_open cs p
                 o <- peek p
                 fptr <- newForeignPtr sqlite3_closeptr o
                 newconn <- mkConn fp fptr auto raw
                 checkError ("connectSqlite3 " ++ fp) fptr res
                 return newconn
         )
        )

mkConn :: FilePath -> Sqlite3 -> Bool -> Bool -> IO Impl.Connection
mkConn fp obj auto raw = do
       children <- if auto
                      then Just <$> newMVar []
                      else return Nothing
       fexecuteRaw obj "BEGIN"

       let alltables = "SELECT name\
                      \ FROM sqlite_master\
                      \ WHERE type='table'\
                      \ ORDER BY name"

       ver <- (sqlite3_libversion >>= peekCString)
       return $ Impl.Connection {
                            Impl.disconnect = fdisconnect obj children,
                            Impl.commit = newtransaction obj "COMMIT",
                            Impl.rollback = newtransaction obj "ROLLBACK",
                            Impl.run = frun obj,
                            Impl.runRaw = fexecuteRaw obj,
                            Impl.prepare = newSth obj children auto,
                            Impl.clone = connectSqlite3Ext auto raw fp,
                            Impl.hdbcDriverName = "sqlite3",
                            Impl.hdbcClientVer = ver,
                            Impl.proxiedClientName = "sqlite3",
                            Impl.proxiedClientVer = ver,
                            Impl.dbTransactionSupport = True,
                            Impl.dbServerVer = ver,
                            Impl.getTables = fgettables obj alltables,
                            Impl.describeTable = fdescribeTable obj,
                            Impl.setBusyTimeout = fsetbusy obj }

fgettables :: Sqlite3 -> String -> IO [String]
fgettables obj query =
    bracket (newSth obj Nothing False query)
             (finish) $ \sth -> do
        res <- execute sth [] >> fetchAllRows' sth
        return $ map fromSql $ concat res

fdescribeTable :: Sqlite3 -> String -> IO [(String, SqlColDesc)]
fdescribeTable o name = do
    sth <- newSth o Nothing False $ "PRAGMA table_info(" ++ name ++ ")"
    res <- execute sth [] *> fetchAllRows' sth <* finish sth
    return [ (fromSql nm, describeType typ notnull df pk)
               | (_:nm:typ:notnull:df:pk:_) <- res ]
  where
     describeType nm notnull _ _ =
         SqlColDesc (typeId nm) Nothing Nothing Nothing (nullable notnull)

     nullable SqlNull = Nothing
     nullable (SqlString "0") = Just True
     nullable (SqlString "1") = Just False
     nullable (SqlByteString x)
       | BUTF8.toString x == "0" = Just True
       | BUTF8.toString x == "1" = Just False
     nullable _ = Nothing

     typeId SqlNull                     = SqlUnknownT "Any"
     typeId (SqlString t)               = typeId' t
     typeId (SqlByteString t)           = typeId' $ BUTF8.toString t
     typeId _                           = SqlUnknownT "Unknown"

     typeId' t = case map Data.Char.toLower t of
       ('i':'n':'t':_) -> SqlIntegerT
       "text"          -> SqlVarCharT
       "real"          -> SqlRealT
       "blob"          -> SqlVarBinaryT
       ""              -> SqlUnknownT "Any"
       other           -> SqlUnknownT other


fsetbusy :: Sqlite3 -> CInt -> IO ()
fsetbusy o ms = withRawSqlite3 o $ \ppdb ->
    sqlite3_busy_timeout ppdb ms

--------------------------------------------------
-- Guts here
--------------------------------------------------

frun :: Sqlite3 -> String -> [SqlValue] -> IO Integer
frun o query args =
    bracket (newSth o Nothing False query)
            (finish)
            (flip execute args)

newtransaction :: Sqlite3 -> String -> IO ()
newtransaction obj how = fexecuteRaw obj how >> fexecuteRaw obj "BEGIN"

fdisconnect :: Sqlite3 -> Maybe ChildList -> IO ()
fdisconnect o mchildren =
    withRawSqlite3 o $ \p -> do
       mapM_ closeAllChildren mchildren
       r <- sqlite3_close p
       checkError "disconnect" o r

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_open2"
  sqlite3_open :: CString -> (Ptr (Ptr CSqlite3)) -> IO CInt

foreign import ccall unsafe "hdbc-sqlite3-helper.h &sqlite3_close_finalizer"
  sqlite3_closeptr :: FunPtr ((Ptr CSqlite3) -> IO ())

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_close_app"
  sqlite3_close :: Ptr CSqlite3 -> IO CInt

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_busy_timeout2"
  sqlite3_busy_timeout :: Ptr CSqlite3 -> CInt -> IO ()

foreign import ccall unsafe "sqlite3.h sqlite3_libversion"
  sqlite3_libversion :: IO CString
