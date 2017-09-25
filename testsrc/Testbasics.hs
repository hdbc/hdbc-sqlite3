module Testbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import Control.Exception
import Control.Monad (when)

openClosedb :: Bool -> Test
openClosedb auto = sqlTestCase $
    do dbh <- connectDBExt auto
       disconnect dbh

multiFinish :: Bool -> Test
multiFinish auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       r <- execute sth []
       assertEqual "basic count" 0 r
       finish sth >> finish sth >> finish sth
                          )

basicQueries :: Bool -> Test
basicQueries auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       execute sth [] >>= (0 @=?)
       r <- fetchAllRows sth
       assertEqual "converted from" [["2"]] (map (map fromSql) r)
       assertEqual "int32 compare" [[SqlInt32 2]] r
       assertEqual "iToSql compare" [[iToSql 2]] r
       assertEqual "num compare" [[toSql (2::Int)]] r
       assertEqual "nToSql compare" [[nToSql (2::Int)]] r
       assertEqual "string compare" [[SqlString "2"]] r
       when (not auto) $ finish sth
                          )

createTable :: Bool -> Test
createTable auto = dbTestCaseExt auto (\dbh ->
    do runRaw dbh "CREATE TABLE hdbctest1 (testname VARCHAR(20), testid INTEGER, testint INTEGER, testtext TEXT)"
       commit dbh
                         )

dropTable :: Bool -> Test
dropTable auto = dbTestCaseExt auto (\dbh ->
    do runRaw dbh "DROP TABLE hdbctest1"
       commit dbh
                       )

runReplace :: Bool -> Test
runReplace auto = dbTestCaseExt auto (\dbh ->
    do r <- run dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r1
       assertEqual "insert retval" 1 r
       _ <- run dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r2
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = 'runReplace' ORDER BY testid"
       rv2 <- execute sth []
       assertEqual "select retval" 0 rv2
       r' <- fetchAllRows sth
       assertEqual "" [r1, r2] r'
       when (not auto) $ finish sth
                       )
    where r1 = [toSql "runReplace", iToSql 1, iToSql 1234, SqlString "testdata"]
          r2 = [toSql "runReplace", iToSql 2, iToSql 2, SqlNull]

executeReplace :: Bool -> Test
executeReplace auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('executeReplace',?,?,?)"
       _ <- execute sth [iToSql 1, iToSql 1234, toSql "Foo"]
       _ <- execute sth [SqlInt32 2, SqlNull, toSql "Bar"]
       when (not auto) $ finish sth
       commit dbh
       sth' <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = ? ORDER BY testid"
       _ <- execute sth' [SqlString "executeReplace"]
       r <- fetchAllRows sth'
       assertEqual "result"
                   [[toSql "executeReplace", iToSql 1, toSql "1234",
                     toSql "Foo"],
                    [toSql "executeReplace", iToSql 2, SqlNull,
                     toSql "Bar"]]
                   r
       when (not auto) $ finish sth'
                            )

testExecuteMany :: Bool -> Test
testExecuteMany auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('multi',?,?,?)"
       executeMany sth rows
       commit dbh
       when (not auto) $ finish sth
       sth' <- prepare dbh "SELECT testid, testint, testtext FROM hdbctest1 WHERE testname = 'multi'"
       _ <- execute sth' []
       r <- fetchAllRows sth'
       assertEqual "" rows r
       when (not auto) $ finish sth'
                          )
    where rows = [map toSql ["1", "1234", "foo"],
                  map toSql ["2", "1341", "bar"],
                  [toSql "3", SqlNull, SqlNull]]

testFetchAllRows :: Bool -> Test
testFetchAllRows auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('fetchAllRows', ?, NULL, NULL)"
       executeMany sth rows
       commit dbh
       when (not auto) $ finish sth
       sth' <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'fetchAllRows' ORDER BY testid"
       _ <- execute sth' []
       results <- fetchAllRows sth'
       assertEqual "" rows results
       when (not auto) $ finish sth'
                               )
    where rows = map (\x -> [iToSql x]) [1..9]

testFetchAllRows' :: Bool -> Test
testFetchAllRows' auto = dbTestCaseExt auto (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('fetchAllRows2', ?, NULL, NULL)"
       executeMany sth rows
       commit dbh
       when (not auto) $ finish sth
       sth' <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'fetchAllRows2' ORDER BY testid"
       _ <- execute sth' []
       results <- fetchAllRows' sth'
       when (not auto) $ finish sth'
       assertEqual "" rows results
                               )
    where rows = map (\x -> [iToSql x]) [1..9]

basicTransactions :: Bool -> Test
basicTransactions auto = dbTestCaseExt auto (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('basicTransactions', ?, NULL, NULL)"
       _ <- execute sth [iToSql 0]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'basicTransactions' ORDER BY testid"
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "initial commit" [[toSql "0"]])

       -- Now try a rollback
       executeMany sth rows
       rollback dbh
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "rollback" [[toSql "0"]])

       -- Now try another commit
       executeMany sth rows
       commit dbh
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "final commit" ([SqlString "0"]:rows))
       when (not auto) $ finish sth >> finish qrysth
                               )
    where rows = map (\x -> [iToSql $ x]) [1..9]

testWithTransaction :: Bool -> Test
testWithTransaction auto = dbTestCaseExt auto (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('withTransaction', ?, NULL, NULL)"
       _ <- execute sth [toSql "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'withTransaction' ORDER BY testid"
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "initial commit" [[toSql "0"]])

       -- Let's try a rollback.
       catch (withTransaction dbh (\_ -> do executeMany sth rows
                                            fail "Foo"))
             ( (\_ -> return ()) :: SomeException -> IO () )
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "rollback" [[SqlString "0"]])

       -- And now a commit.
       withTransaction dbh (\_ -> executeMany sth rows)
       _ <- execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "final commit" ([iToSql 0]:rows))
       when (not auto) $ finish sth >> finish qrysth
                               )
    where rows = map (\x -> [iToSql x]) [1..9]

autoTests :: Bool -> Test
autoTests auto = TestList
    [ TestLabel "openClosedb" (openClosedb auto)
    , TestLabel "multiFinish" (multiFinish auto)
    , TestLabel "basicQueries" (basicQueries auto)
    , TestLabel "createTable" (createTable auto)
    , TestLabel "runReplace" (runReplace auto)
    , TestLabel "executeReplace" (executeReplace auto)
    , TestLabel "executeMany" (testExecuteMany auto)
    , TestLabel "fetchAllRows" (testFetchAllRows auto)
    , TestLabel "fetchAllRows'" (testFetchAllRows' auto)
    , TestLabel "basicTransactions" (basicTransactions auto)
    , TestLabel "withTransaction" (testWithTransaction auto)
    , TestLabel "dropTable" (dropTable True)
    ]

tests :: Test
tests = TestList
    [ TestLabel "Auto-finish true tests" (autoTests True)
    , TestLabel "Auto-finish false tests" (autoTests False)
    ]
