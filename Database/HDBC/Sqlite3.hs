{- |
   Module     : Database.HDBC.Sqlite3
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

HDBC driver interface for Sqlite 3.x.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Sqlite3
    (
    -- * Sqlite3 Basics
     connectSqlite3, connectSqlite3Raw, Connection(), setBusyTimeout,
    -- * Sqlite3 Error Consts
    module Database.HDBC.Sqlite3.Consts
    )

where

import Database.HDBC.Sqlite3.Connection(connectSqlite3, connectSqlite3Raw, Connection())
import Database.HDBC.Sqlite3.ConnectionImpl(setBusyTimeout)
import Database.HDBC.Sqlite3.Consts
