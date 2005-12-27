module SpecificDB where
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.HUnit

connectDB = 
    handleSqlError (connectSqlite3 "testtmp.sql3")

tests = TestLabel "Sqlite3 tests" $ TestList []