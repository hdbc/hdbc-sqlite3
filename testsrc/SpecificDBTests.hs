module SpecificDBTests where
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.HUnit
import TestMisc(setup)

testgetTables = setup $ \dbh ->
    do r <- getTables dbh
       ["hdbctest2"] @=? r

tests = TestList [TestLabel "getTables" testgetTables]
