module SpecificDBTests where
import Database.HDBC
import Test.HUnit
import TestMisc(setup)

testgetTables :: Bool -> Test
testgetTables auto = setup auto $ \dbh ->
    do r <- getTables dbh
       ["hdbctest2"] @=? r

tests :: Test
tests = TestList [ TestLabel "getTables auto-finish on"  (testgetTables True)
                 , TestLabel "getTables auto-finish off" (testgetTables False)
                 ]
