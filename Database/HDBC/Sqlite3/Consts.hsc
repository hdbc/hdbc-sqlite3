{- -*- mode: haskell; -*- 
   vim: set filetype=haskell:
Copyright (C) 2005-2007 John Goerzen <jgoerzen@complete.org>

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

module Database.HDBC.Sqlite3.Consts
 (sqlite_OK) where

import Foreign.C.Types

#include <sqlite3.h>

sqlite_OK :: Int -- | Successful result
sqlite_OK = #{const SQLITE_OK}

sqlite_ERROR :: Int -- | SQL error or missing database
sqlite_ERROR = #{const SQLITE_ERROR}

sqlite_INTERNAL :: Int -- | An internal logic error in SQLite
sqlite_INTERNAL = #{const SQLITE_INTERNAL}

sqlite_PERM :: Int -- | Access permission denied
sqlite_PERM = #{const SQLITE_PERM}

sqlite_ABORT :: Int -- | Callback routine requested an abort
sqlite_ABORT = #{const SQLITE_ABORT}

sqlite_BUSY :: Int -- | The database file is locked
sqlite_BUSY = #{const SQLITE_BUSY}

sqlite_LOCKED :: Int -- | A table in the database is locked
sqlite_LOCKED = #{const SQLITE_LOCKED}

sqlite_NOMEM :: Int -- | A malloc() failed
sqlite_NOMEM = #{const SQLITE_NOMEM}

sqlite_READONLY :: Int -- | Attempt to write a readonly database
sqlite_READONLY = #{const SQLITE_READONLY}

sqlite_INTERRUPT :: Int -- | Operation terminated by sqlite_interrupt()
sqlite_INTERRUPT = #{const SQLITE_INTERRUPT}

sqlite_IOERR :: Int -- | Some kind of disk I/O error occurred
sqlite_IOERR = #{const SQLITE_IOERR}

sqlite_CORRUPT :: Int -- | The database disk image is malformed
sqlite_CORRUPT = #{const SQLITE_CORRUPT}

sqlite_NOTFOUND :: Int -- | (Internal Only) Table or record not found
sqlite_NOTFOUND = #{const SQLITE_NOTFOUND}

sqlite_FULL :: Int -- | Insertion failed because database is full
sqlite_FULL = #{const SQLITE_FULL}

sqlite_CANTOPEN :: Int -- | Unable to open the database file
sqlite_CANTOPEN = #{const SQLITE_CANTOPEN}

sqlite_PROTOCOL :: Int -- | Database lock protocol error
sqlite_PROTOCOL = #{const SQLITE_PROTOCOL}

sqlite_EMPTY :: Int -- | (Internal Only) Database table is empty
sqlite_EMPTY = #{const SQLITE_EMPTY}

sqlite_SCHEMA :: Int -- | The database schema changed
sqlite_SCHEMA = #{const SQLITE_SCHEMA}

sqlite_TOOBIG :: Int -- | Too much data for one row of a table
sqlite_TOOBIG = #{const SQLITE_TOOBIG}

sqlite_CONSTRAINT :: Int -- | Abort due to constraint violation
sqlite_CONSTRAINT = #{const SQLITE_CONSTRAINT}

sqlite_MISMATCH :: Int -- | Data type mismatch
sqlite_MISMATCH = #{const SQLITE_MISMATCH}

sqlite_MISUSE :: Int -- | Library used incorrectly
sqlite_MISUSE = #{const SQLITE_MISUSE}

sqlite_NOLFS :: Int -- | Uses OS features not supported on host
sqlite_NOLFS = #{const SQLITE_NOLFS}

sqlite_AUTH :: Int -- | Authorization denied
sqlite_AUTH = #{const SQLITE_AUTH}

sqlite_ROW :: Int -- | sqlite_step() has another row ready
sqlite_ROW = #{const SQLITE_ROW}

sqlite_DONE :: Int -- | sqlite_step() has finished executing
sqlite_DONE = #{const SQLITE_DONE}

