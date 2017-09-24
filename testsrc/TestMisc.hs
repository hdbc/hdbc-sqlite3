module TestMisc(tests, setup) where
import Test.HUnit
import Database.HDBC
import Database.HDBC.Sqlite3
import TestUtils
import Control.Exception
import Data.Char
import Control.Monad
import qualified Data.Map as Map

rowdata :: [[SqlValue]]
rowdata = 
    [[SqlInt32 0, toSql "Testing", SqlNull],
     [SqlInt32 1, toSql "Foo", SqlInt32 5],
     [SqlInt32 2, toSql "Bar", SqlInt32 9]]

colnames :: [String]
colnames = ["testid", "teststring", "testint"]
alrows :: [[(String, SqlValue)]]
alrows = map (zip colnames) rowdata

setup :: Bool -> (Connection -> IO ()) -> Test
setup auto f = dbTestCaseExt auto $ \dbh ->
   do _ <- run dbh "CREATE TABLE hdbctest2 (testid INTEGER PRIMARY KEY NOT NULL, teststring TEXT, testint INTEGER)" []
      sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
      executeMany sth rowdata
      when (not auto) $ finish sth
      commit dbh
      finally (f dbh)
              (do _ <- run dbh "DROP TABLE hdbctest2" []
                  commit dbh
              )

safeQuickQuery' :: Connection -> String -> [SqlValue] -> IO [[SqlValue]]
safeQuickQuery' conn query args = do
    bracket (prepare conn query)
            (finish) $ \sth -> do
        _ <- execute sth args
        fetchAllRows' sth

cloneTest :: forall b conn. IConnection conn =>
             conn -> (conn -> IO b) -> IO b
cloneTest dbh a =
    do dbh2 <- clone dbh
       finally (handleSqlError (a dbh2))
               (handleSqlError (disconnect dbh2))

testgetColumnNames :: Bool -> Test
testgetColumnNames auto = setup auto $ \dbh ->
   do sth <- prepare dbh "SELECT * from hdbctest2"
      _ <- execute sth []
      cols <- getColumnNames sth
      finish sth
      ["testid", "teststring", "testint"] @=? map (map toLower) cols

testdescribeResult :: Bool -> Test
testdescribeResult auto = setup auto $ \dbh -> when (not ((hdbcDriverName dbh) `elem`
                                               ["sqlite3"])) $
   do sth <- prepare dbh "SELECT * from hdbctest2"
      _ <- execute sth []
      cols <- describeResult sth
      ["testid", "teststring", "testint"] @=? map (map toLower . fst) cols
      let coldata = map snd cols
      assertBool "r0 type" (colType (coldata !! 0) `elem`
                            [SqlBigIntT, SqlIntegerT])
      assertBool "r1 type" (colType (coldata !! 1) `elem`
                            [SqlVarCharT, SqlLongVarCharT])
      assertBool "r2 type" (colType (coldata !! 2) `elem`
                            [SqlBigIntT, SqlIntegerT])
      finish sth

testdescribeTable :: Bool -> Test
testdescribeTable auto = setup auto $ \dbh -> when (not ((hdbcDriverName dbh) `elem`
                                               ["sqlite3"])) $
   do cols <- describeTable dbh "hdbctest2"
      ["testid", "teststring", "testint"] @=? map (map toLower . fst) cols
      let coldata = map snd cols
      assertBool "r0 type" (colType (coldata !! 0) `elem`
                            [SqlBigIntT, SqlIntegerT])
      assertEqual "r0 nullable" (Just False) (colNullable (coldata !! 0))
      assertBool "r1 type" (colType (coldata !! 1) `elem`
                            [SqlVarCharT, SqlLongVarCharT])
      assertEqual "r1 nullable" (Just True) (colNullable (coldata !! 1))
      assertBool "r2 type" (colType (coldata !! 2) `elem`
                           [SqlBigIntT, SqlIntegerT])
      assertEqual "r2 nullable" (Just True) (colNullable (coldata !! 2))

-- Quick query creates a hidden prepared statement in the parent HDBC
-- library, and is not suitable for use without auto-finish.
testquickQuery :: Bool -> Test
testquickQuery _ = setup True $ \dbh ->
    do results <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

testfetchRowAL :: Bool -> Test
testfetchRowAL auto = setup auto $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       _ <- execute sth []
       fetchRowAL sth >>= (Just (head alrows) @=?)
       fetchRowAL sth >>= (Just (alrows !! 1) @=?)
       fetchRowAL sth >>= (Just (alrows !! 2) @=?)
       fetchRowAL sth >>= (Nothing @=?)
       finish sth

testfetchRowMap :: Bool -> Test
testfetchRowMap auto = setup auto $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       _ <- execute sth []
       fetchRowMap sth >>= (Just (Map.fromList $ head alrows) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 1) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 2) @=?)
       fetchRowMap sth >>= (Nothing @=?)
       finish sth

testfetchAllRowsAL :: Bool -> Test
testfetchAllRowsAL auto = setup auto $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       _ <- execute sth []
       fetchAllRowsAL sth >>= (alrows @=?)
       when (not auto) $ finish sth

testfetchAllRowsMap :: Bool -> Test
testfetchAllRowsMap auto = setup auto $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       _ <- execute sth []
       fetchAllRowsMap sth >>= (map (Map.fromList) alrows @=?)
       when (not auto) $ finish sth

testexception :: Bool -> Test
testexception auto = setup auto $ \dbh ->
    catchSql (do bracket (prepare dbh "SELECT invalidcol FROM hdbctest2")
                         (finish)
                         (flip execute []) >> return ()
                 assertFailure "No exception was raised"
             )
             (\_ -> commit dbh)

testrowcount :: Bool -> Test
testrowcount auto = setup auto $ \dbh ->
    do r <- run dbh "UPDATE hdbctest2 SET testint = 25 WHERE testid = 20" []
       assertEqual "UPDATE with no change" 0 r
       r' <- run dbh "UPDATE hdbctest2 SET testint = 26 WHERE testid = 0" []
       assertEqual "UPDATE with 1 change" 1 r'
       r'' <- run dbh "UPDATE hdbctest2 SET testint = 27 WHERE testid <> 0" []
       assertEqual "UPDATE with 2 changes" 2 r''
       commit dbh
       res <- safeQuickQuery' dbh "SELECT * from hdbctest2 ORDER BY testid" []
       assertEqual "final results"
         [[SqlInt32 0, toSql "Testing", SqlInt32 26],
          [SqlInt32 1, toSql "Foo", SqlInt32 27],
          [SqlInt32 2, toSql "Bar", SqlInt32 27]] res
                       
{- Since we might be running against a live DB, we can't look at a specific
list here (though a SpecificDB test case may be able to).  We can ensure
that our test table is, or is not, present, as appropriate. -}
                                      
testgetTables1 :: Bool -> Test
testgetTables1 auto = setup auto $ \dbh ->
    do r <- getTables dbh
       True @=? "hdbctest2" `elem` r

testgetTables2 :: Bool -> Test
testgetTables2 auto = dbTestCaseExt auto $ \dbh ->
    do r <- getTables dbh
       False @=? "hdbctest2" `elem` r

testclone :: Bool -> Test
testclone auto = setup auto $ \dbho -> cloneTest dbho $ \dbh ->
    do results <- safeQuickQuery' dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

testnulls :: Bool -> Test
testnulls auto = setup auto $ \dbh ->
    do let dn = hdbcDriverName dbh
       when (not (dn `elem` ["postgresql", "odbc"])) (
          do sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
             executeMany sth rows
             finish sth
             res <- safeQuickQuery' dbh "SELECT * from hdbctest2 WHERE testid > 99 ORDER BY testid" []
             rows @=? res
                                             )
    where rows = [[SqlInt32 100, SqlString "foo\NULbar", SqlNull],
                  [SqlInt32 101, SqlString "bar\NUL", SqlNull],
                  [SqlInt32 102, SqlString "\NUL", SqlNull],
                  [SqlInt32 103, SqlString "\xFF", SqlNull],
                  [SqlInt32 104, SqlString "regular", SqlNull]]
       
testunicode :: Bool -> Test
testunicode auto = setup auto $ \dbh ->
      do sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
         executeMany sth rows
         finish sth
         res <- safeQuickQuery' dbh "SELECT * from hdbctest2 WHERE testid > 99 ORDER BY testid" []
         rows @=? res
    where rows = [[SqlInt32 100, SqlString "foo\x263a", SqlNull],
                  [SqlInt32 101, SqlString "bar\x00A3", SqlNull],
                  [SqlInt32 102, SqlString (take 263 (repeat 'a')), SqlNull]]

autoTests :: Bool -> Test
autoTests auto = TestList
    [ TestLabel "getColumnNames" (testgetColumnNames auto)
    , TestLabel "describeResult" (testdescribeResult auto)
    , TestLabel "describeTable" (testdescribeTable auto)
    , TestLabel "quickQuery" (testquickQuery auto)
    , TestLabel "fetchRowAL" (testfetchRowAL auto)
    , TestLabel "fetchRowMap" (testfetchRowMap auto)
    , TestLabel "fetchAllRowsAL" (testfetchAllRowsAL auto)
    , TestLabel "fetchAllRowsMap" (testfetchAllRowsMap auto)
    , TestLabel "sql exception" (testexception auto)
    , TestLabel "clone" (testclone auto)
    , TestLabel "update rowcount" (testrowcount auto)
    , TestLabel "get tables1" (testgetTables1 auto)
    , TestLabel "get tables2" (testgetTables2 auto)
    , TestLabel "nulls" (testnulls auto)
    , TestLabel "unicode" (testunicode auto)
    ]

tests :: Test
tests = TestList [ TestLabel "auto-finish on"  (autoTests True)
                 , TestLabel "auto-finish off" (autoTests False)
                 ]
