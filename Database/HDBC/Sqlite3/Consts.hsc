{- -*- mode: haskell; -*- 
   vim: set filetype=haskell:
-}

module Database.HDBC.Sqlite3.Consts
 (sqlite_OK,
  sqlite_ERROR,
  sqlite_INTERNAL,
  sqlite_PERM,
  sqlite_ABORT,
  sqlite_BUSY,
  sqlite_LOCKED,
  sqlite_NOMEM,
  sqlite_READONLY,
  sqlite_INTERRUPT,
  sqlite_IOERR,
  sqlite_CORRUPT,
  sqlite_NOTFOUND,
  sqlite_FULL,
  sqlite_CANTOPEN,
  sqlite_PROTOCOL,
  sqlite_EMPTY,
  sqlite_SCHEMA,
  sqlite_TOOBIG,
  sqlite_CONSTRAINT,
  sqlite_MISMATCH,
  sqlite_MISUSE,
  sqlite_NOLFS,
  sqlite_AUTH,
  sqlite_ROW,
  sqlite_DONE)
where

import Foreign.C.Types

#include <sqlite3.h>

-- | Successful result
sqlite_OK :: Int
sqlite_OK = #{const SQLITE_OK}

-- | SQL error or missing database
sqlite_ERROR :: Int
sqlite_ERROR = #{const SQLITE_ERROR}

-- | An internal logic error in SQLite
sqlite_INTERNAL :: Int
sqlite_INTERNAL = #{const SQLITE_INTERNAL}

-- | Access permission denied
sqlite_PERM :: Int
sqlite_PERM = #{const SQLITE_PERM}

-- | Callback routine requested an abort
sqlite_ABORT :: Int
sqlite_ABORT = #{const SQLITE_ABORT}

-- | The database file is locked
sqlite_BUSY :: Int
sqlite_BUSY = #{const SQLITE_BUSY}

-- | A table in the database is locked
sqlite_LOCKED :: Int
sqlite_LOCKED = #{const SQLITE_LOCKED}

-- | A malloc() failed
sqlite_NOMEM :: Int
sqlite_NOMEM = #{const SQLITE_NOMEM}

-- | Attempt to write a readonly database
sqlite_READONLY :: Int
sqlite_READONLY = #{const SQLITE_READONLY}

-- | Operation terminated by sqlite_interrupt()
sqlite_INTERRUPT :: Int
sqlite_INTERRUPT = #{const SQLITE_INTERRUPT}

-- | Some kind of disk I\/O error occurred
sqlite_IOERR :: Int
sqlite_IOERR = #{const SQLITE_IOERR}

-- | The database disk image is malformed
sqlite_CORRUPT :: Int
sqlite_CORRUPT = #{const SQLITE_CORRUPT}

-- | (Internal Only) Table or record not found
sqlite_NOTFOUND :: Int
sqlite_NOTFOUND = #{const SQLITE_NOTFOUND}

-- | Insertion failed because database is full
sqlite_FULL :: Int
sqlite_FULL = #{const SQLITE_FULL}

-- | Unable to open the database file
sqlite_CANTOPEN :: Int
sqlite_CANTOPEN = #{const SQLITE_CANTOPEN}

-- | Database lock protocol error
sqlite_PROTOCOL :: Int
sqlite_PROTOCOL = #{const SQLITE_PROTOCOL}

-- | (Internal Only) Database table is empty
sqlite_EMPTY :: Int
sqlite_EMPTY = #{const SQLITE_EMPTY}

-- | The database schema changed
sqlite_SCHEMA :: Int
sqlite_SCHEMA = #{const SQLITE_SCHEMA}

-- | Too much data for one row of a table
sqlite_TOOBIG :: Int
sqlite_TOOBIG = #{const SQLITE_TOOBIG}

-- | Abort due to constraint violation
sqlite_CONSTRAINT :: Int
sqlite_CONSTRAINT = #{const SQLITE_CONSTRAINT}

-- | Data type mismatch
sqlite_MISMATCH :: Int
sqlite_MISMATCH = #{const SQLITE_MISMATCH}

-- | Library used incorrectly
sqlite_MISUSE :: Int
sqlite_MISUSE = #{const SQLITE_MISUSE}

-- | Uses OS features not supported on host
sqlite_NOLFS :: Int
sqlite_NOLFS = #{const SQLITE_NOLFS}

-- | Authorization denied
sqlite_AUTH :: Int
sqlite_AUTH = #{const SQLITE_AUTH}

-- | sqlite_step() has another row ready
sqlite_ROW :: Int
sqlite_ROW = #{const SQLITE_ROW}

-- | sqlite_step() has finished executing
sqlite_DONE :: Int
sqlite_DONE = #{const SQLITE_DONE}

