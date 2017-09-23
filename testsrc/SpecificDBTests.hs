module SpecificDBTests where
import Database.HDBC
import Test.HUnit
import TestMisc(setup)

testgetTables :: Test
testgetTables = setup $ \dbh ->
    do r <- getTables dbh
       ["hdbctest2"] @=? r

tests :: Test
tests = TestList [TestLabel "getTables" testgetTables]
