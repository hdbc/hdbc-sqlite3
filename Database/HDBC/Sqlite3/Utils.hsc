{- -*- mode: haskell; -*- 
   vim: set filetype=haskell:
-}

module Database.HDBC.Sqlite3.Utils where
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Database.HDBC(throwSqlError)
import Database.HDBC.Types
import Database.HDBC.Sqlite3.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8
import Foreign.C.Types
import Foreign.Storable

#include "hdbc-sqlite3-helper.h"

checkError :: String -> Sqlite3 -> CInt -> IO ()
checkError _ _ 0 = return ()
checkError msg o res =
    withSqlite3 o
     (\p -> do rc <- sqlite3_errmsg p
               bs <- B.packCString rc
               let str = BUTF8.toString bs
               throwSqlError $
                          SqlError {seState = "",
                                    seNativeError = fromIntegral res,
                                    seErrorMsg = msg ++ ": " ++ str}
     )

{- This is a little hairy.

We have a CSqlite3 object that is actually a finalizeonce wrapper around
the real object.  We use withSqlite3 to dereference the foreign pointer,
and then extract the pointer to the real object from the finalizeonce struct.

But, when we close the connection, we need the finalizeonce struct, so that's
done by withRawSqlite3.

Ditto for statements. -}

withSqlite3 :: Sqlite3 -> (Ptr CSqlite3 -> IO b) -> IO b
withSqlite3 = genericUnwrap

withRawSqlite3 :: Sqlite3 -> (Ptr CSqlite3 -> IO b) -> IO b
withRawSqlite3 = withForeignPtr

withStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withStmt = genericUnwrap

withRawStmt :: Stmt -> (Ptr CStmt -> IO b) -> IO b
withRawStmt = withForeignPtr


genericUnwrap :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
genericUnwrap fptr action = withForeignPtr fptr (\structptr ->
    do objptr <- #{peek finalizeonce, encapobj} structptr
       action objptr
                                                )

foreign import ccall unsafe "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg :: (Ptr CSqlite3) -> IO CString

