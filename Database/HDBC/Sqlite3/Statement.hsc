-- -*- mode: haskell; -*-
{-# CFILES hdbc-sqlite3-helper.c #-}
-- Above line for Hugs
module Database.HDBC.Sqlite3.Statement where
import Database.HDBC.Types
import Database.HDBC
import Database.HDBC.Sqlite3.Types
import Database.HDBC.Sqlite3.Utils
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Control.Concurrent.MVar
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import Data.List
import Data.Int (Int64)
import Database.HDBC.DriverUtils

#include <sqlite3.h>

{- One annoying thing about Sqlite is that a disconnect operation will actually
fail if there are any active statements.  This is highly annoying, and makes
for some somewhat complex algorithms. -}

data StoState = Empty           -- ^ Not initialized or last execute\/fetchrow had no results
              | Prepared Stmt   -- ^ Prepared but not executed
              | Executed Stmt   -- ^ Executed and more rows are expected
              | Exhausted Stmt  -- ^ Executed and at end of rows

instance Show StoState where
    show Empty = "Empty"
    show (Prepared _) = "Prepared"
    show (Executed _) = "Executed"
    show (Exhausted _) = "Exhausted"

data SState = SState {dbo :: Sqlite3,
                      stomv :: MVar StoState,
                      querys :: String,
                      colnamesmv :: MVar [String],
                      autoFinish :: Bool}

newSth :: Sqlite3 -> ChildList -> Bool -> String -> IO Statement
newSth indbo mchildren autoFinish str = 
    do newstomv <- newMVar Empty
       newcolnamesmv <- newMVar []
       let sstate = SState{dbo = indbo,
                           stomv = newstomv,
                           querys = str,
                           colnamesmv = newcolnamesmv,
                           autoFinish = autoFinish}
       modifyMVar_ (stomv sstate) (\_ -> (fprepare sstate >>= return . Prepared))
       let retval = 
               Statement {execute = fexecute sstate,
                           executeRaw = fexecuteRaw indbo str,
                           executeMany = fexecutemany sstate,
                           finish = public_ffinish sstate,
                           fetchRow = ffetchrow sstate,
                           originalQuery = str,
                           getColumnNames = readMVar (colnamesmv sstate),
                           describeResult = fail "Sqlite3 backend does not support describeResult"}
       addChild mchildren retval
       return retval

{- The deal with adding the \0 below is in response to an apparent bug in
sqlite3.  See debian bug #343736. 

This function assumes that any existing query in the state has already
been terminated.  (FIXME: should check this at runtime.... never run fprepare
unless state is Empty)
-}
fprepare :: SState -> IO Stmt
fprepare sstate = withRawSqlite3 (dbo sstate)
  (\p -> B.useAsCStringLen (BUTF8.fromString ((querys sstate) ++ "\0"))
   (\(cs, cslen) -> alloca
    (\(newp::Ptr (Ptr CStmt)) -> 
     (do res <- sqlite3_prepare p cs (fromIntegral cslen) newp nullPtr
         checkError ("prepare " ++ (show cslen) ++ ": " ++ (querys sstate)) 
                    (dbo sstate) res
         newo <- peek newp
         newForeignPtr sqlite3_finalizeptr newo
     )
     )
   )
   )
                 

{- General algorithm: find out how many columns we have, check the type
of each to see if it's NULL.  If it's not, fetch it as text and return that.

Note that execute() will have already loaded up the first row -- and we
do that each time.  so this function returns the row that is already in sqlite,
then loads the next row. -}
ffetchrow :: SState -> IO (Maybe [SqlValue])
ffetchrow sstate = modifyMVar (stomv sstate) dofetchrow
    where dofetchrow Empty = return (Empty, Nothing)
          dofetchrow (Prepared _) = 
              throwSqlError $ SqlError {seState = "HDBC Sqlite3 fetchrow",
                                   seNativeError = (-1),
                                   seErrorMsg = "Attempt to fetch row from Statement that has not been executed.  Query was: " ++ (querys sstate)}
          dofetchrow (Executed sto) = withStmt sto (\p ->
              do ccount <- sqlite3_column_count p
                 -- fetch the data
                 res <- mapM (getCol p) [0..(ccount - 1)]
                 r <- fstep (dbo sstate) p
                 if r
                    then return (Executed sto, Just res)
                    else if (autoFinish sstate)
                            then do ffinish (dbo sstate) sto
                                    return (Empty, Just res)
                            else return (Exhausted sto, Just res)
                                                          )
          dofetchrow (Exhausted sto) = return (Exhausted sto, Nothing)
 
          getCol p icol = 
             do t <- sqlite3_column_type p icol
                case t of
                  #{const SQLITE_NULL}    -> return SqlNull
                  #{const SQLITE_INTEGER} -> SqlInt64  <$> sqlite3_column_int64  p icol
                  #{const SQLITE_FLOAT}   -> SqlDouble <$> sqlite3_column_double p icol
                  _                       -> SqlByteString <$> getbytes p icol

          getbytes p icol =
             do str <- sqlite3_column_text  p icol
                len <- sqlite3_column_bytes p icol
                B.packCStringLen (str, fromIntegral len)


fstep :: Sqlite3 -> Ptr CStmt -> IO Bool
fstep dbo p =
    do r <- sqlite3_step p
       case r of
         #{const SQLITE_ROW} -> return True
         #{const SQLITE_DONE} -> return False
         #{const SQLITE_ERROR} -> checkError "step" dbo #{const SQLITE_ERROR}
                                   >> (throwSqlError $ SqlError 
                                          {seState = "",
                                           seNativeError = 0,
                                           seErrorMsg = "In HDBC step, internal processing error (got SQLITE_ERROR with no error)"})
         x -> checkError "step" dbo x
              >> (throwSqlError $ SqlError 
                                {seState = "",
                                 seNativeError = fromIntegral x,
                                 seErrorMsg = "In HDBC step, internal processing error (got error code with no error)"})

fexecute sstate args = modifyMVar (stomv sstate) doexecute
    where doexecute (Executed sto) = doexecute (Prepared sto)
          doexecute (Exhausted sto) = doexecute (Prepared sto)
          doexecute Empty =     -- already cleaned up from last time
              do sto <- fprepare sstate
                 doexecute (Prepared sto)
          doexecute (Prepared sto) = withStmt sto (\p -> 
              do c <- sqlite3_bind_parameter_count p
                 when (c /= genericLength args)
                   (throwSqlError $ SqlError {seState = "",
                                         seNativeError = (-1),
                                         seErrorMsg = "In HDBC execute, received " ++ (show args) ++ " but expected " ++ (show c) ++ " args."})
                 sqlite3_reset p >>= checkError "execute (reset)" (dbo sstate)
                 zipWithM_ (bindArgs p) [1..c] args

                 {- Logic for handling counts of changes: look at the total
                    changes before and after the query.  If they differ,
                    then look at the local changes.  (The local change counter
                    appears to not be updated unless really running a query
                    that makes a change, according to the docs.)

                    This is OK thread-wise because SQLite doesn't support
                    using a given dbh in more than one thread anyway. -}
                 origtc <- withSqlite3 (dbo sstate) sqlite3_total_changes 
                 r <- fstep (dbo sstate) p
                 newtc <- withSqlite3 (dbo sstate) sqlite3_total_changes
                 changes <- if origtc == newtc
                               then return 0
                               else withSqlite3 (dbo sstate) sqlite3_changes
                 fgetcolnames p >>= swapMVar (colnamesmv sstate)
                 if r
                    then return (Executed sto, fromIntegral changes)
                    else if (autoFinish sstate)
                            then do ffinish (dbo sstate) sto
                                    return (Empty, fromIntegral changes)
                            else return (Exhausted sto, fromIntegral changes)
                                                        )
          bindArgs p i SqlNull =
              sqlite3_bind_null p i >>= 
                checkError ("execute (binding NULL column " ++ (show i) ++ ")")
                           (dbo sstate)
          bindArgs p i (SqlByteString bs) =
              B.useAsCStringLen bs (bindCStringArgs p i)
          bindArgs p i arg = bindArgs p i (SqlByteString (fromSql arg))

          bindCStringArgs p i (cs, len) =
              do r <- sqlite3_bind_text2 p i cs (fromIntegral len)
                 checkError ("execute (binding column " ++ 
                             (show i) ++ ")") (dbo sstate) r

fexecuteRaw :: Sqlite3 -> String -> IO ()
fexecuteRaw dbo query =
    withSqlite3 dbo
      (\p -> B.useAsCStringLen (BUTF8.fromString (query ++ "\0"))
       (\(cs, cslen) -> do
          result <- sqlite3_exec p cs nullFunPtr nullPtr nullPtr
          case result of
            #{const SQLITE_OK} -> return ()
            s -> do
              checkError "exec" dbo s
              throwSqlError $ SqlError
                 {seState = "",
                  seNativeError = fromIntegral s,
                  seErrorMsg = "In sqlite3_exec, internal error"}
       )
      )

fgetcolnames csth =
        do count <- sqlite3_column_count csth
           mapM (getCol csth) [0..(count -1)]
    where getCol csth i =
              do cstr <- sqlite3_column_name csth i
                 bs <- B.packCString cstr
                 return (BUTF8.toString bs)

fexecutemany _ [] = return ()
fexecutemany sstate (args:[]) = 
    do fexecute sstate args
       return ()
fexecutemany sstate (args:arglist) =
    do fexecute (sstate { autoFinish = False }) args
       fexecutemany sstate arglist

--ffinish o = withForeignPtr o (\p -> sqlite3_finalize p >>= checkError "finish")
-- Finish and change state
public_ffinish sstate = modifyMVar_ (stomv sstate) worker
    where worker (Empty) = return Empty
          worker (Prepared sto) = ffinish (dbo sstate) sto >> return Empty
          worker (Executed sto) = ffinish (dbo sstate) sto >> return Empty
          worker (Exhausted sto) = ffinish (dbo sstate) sto >> return Empty
    
ffinish dbo o = withRawStmt o (\p -> do r <- sqlite3_finalize p
                                        checkError "finish" dbo r)

foreign import ccall unsafe "hdbc-sqlite3-helper.h &sqlite3_finalize_finalizer"
  sqlite3_finalizeptr :: FunPtr ((Ptr CStmt) -> IO ())

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_finalize_app"
  sqlite3_finalize :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_prepare2"
  sqlite3_prepare :: (Ptr CSqlite3) -> CString -> CInt -> Ptr (Ptr CStmt) -> Ptr (Ptr CString) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_bind_parameter_count"
  sqlite3_bind_parameter_count :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_step"
  sqlite3_step :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_exec"
  sqlite3_exec :: (Ptr CSqlite3)
               -> CString
               -> FunPtr (Ptr () -> CInt -> Ptr CString -> Ptr CString)
               -> Ptr ()
               -> Ptr CString
               -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_reset"
  sqlite3_reset :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_column_count"
  sqlite3_column_count :: (Ptr CStmt) -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_column_name"
  sqlite3_column_name :: Ptr CStmt -> CInt -> IO CString

foreign import ccall unsafe "sqlite3.h sqlite3_column_type"
  sqlite3_column_type :: (Ptr CStmt) -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_column_text"
  sqlite3_column_text :: (Ptr CStmt) -> CInt -> IO CString

foreign import ccall unsafe "sqlite3.h sqlite3_column_bytes"
  sqlite3_column_bytes :: (Ptr CStmt) -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_column_int64"
  sqlite3_column_int64 :: (Ptr CStmt) -> CInt -> IO Int64

foreign import ccall unsafe "sqlite3.h sqlite3_column_double"
  sqlite3_column_double :: (Ptr CStmt) -> CInt -> IO Double

foreign import ccall unsafe "hdbc-sqlite3-helper.h sqlite3_bind_text2"
  sqlite3_bind_text2 :: (Ptr CStmt) -> CInt -> CString -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_bind_null"
  sqlite3_bind_null :: (Ptr CStmt) -> CInt -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_changes"
  sqlite3_changes :: Ptr CSqlite3 -> IO CInt

foreign import ccall unsafe "sqlite3.h sqlite3_total_changes"
  sqlite3_total_changes :: Ptr CSqlite3 -> IO CInt
