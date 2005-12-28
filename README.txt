Welcome to HDBC, Haskell Database Connectivity.

This package provides a database backend driver for Sqlite version 3.

Please see HDBC itself for documentation on use.  If you don't already
have it, you can browse this documentation at
http://darcs.complete.org/hdbc/doc/index.html.

This package provides one function in module Database.HDBC.Sqlite3:

{- | Connect to an Sqlite version 3 database.  The only parameter needed is
the filename of the database to connect to.

All database accessor functions are provided in the main HDBC module. -}
connectSqlite3 :: FilePath -> IO Connection

PREREQUISITES
-------------

Before installing this package, you'll need to have HDBC 0.99.0 or
above installed.  You can download HDBC from http://quux.org/devel/hdbc.

INSTALLATION
------------

The steps to install are:

1) ghc --make -o setup Setup.lhs

2) ./setup configure

3) ./setup build

4) ./setup install   (as root)

If you're on Windows, you can omit the leading "./".

USAGE
-----

To use with hugs, you'll want to use hugs -98.

To use with GHC, you'll want to use:

 -package HDBC -package HDBC-sqlite3

Or, with Cabal, use:

  Build-Depends: HDBC>=0.99.0, HDBC-sqlite3

-- John Goerzen
   December 2005