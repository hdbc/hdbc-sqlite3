module TestSbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import Control.Exception

openClosedb :: Test
openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

multiFinish :: Test
multiFinish = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       _ <- sExecute sth []
       finish sth
       finish sth
       finish sth
                          )

runRawTest :: Test
runRawTest = dbTestCase (\dbh ->
    do runRaw dbh "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int)"
       tables <- getTables dbh
       assertBool "valid1 table not created!" ("valid1" `elem` tables)
       assertBool "valid2 table not created!" ("valid2" `elem` tables)
                        )


runRawErrorTest :: Test
runRawErrorTest = dbTestCase (\dbh ->
    do err <- (runRaw dbh "CREATE TABLE valid1 (a int); INVALID" >> return "No error") `catchSql`
              (return . seErrorMsg)
       assertEqual "exception text" "exec: near \"INVALID\": syntax error" err
       rollback dbh
       tables <- getTables dbh
       assertBool "valid1 table created!" (not $ "valid1" `elem` tables)
                        )

basicQueries :: Test
basicQueries = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       _ <- sExecute sth []
       sFetchRow sth >>= (assertEqual "row 1" (Just [Just "2"]))
       sFetchRow sth >>= (assertEqual "last row" Nothing)
                          )
    
createTable :: Test
createTable = dbTestCase (\dbh ->
    do _ <- sRun dbh "CREATE TABLE hdbctest1 (testname VARCHAR(20), testid INTEGER, testint INTEGER, testtext TEXT)" []
       commit dbh
                         )

dropTable :: Test
dropTable = dbTestCase (\dbh ->
    do _ <- sRun dbh "DROP TABLE hdbctest1" []
       commit dbh
                       )

runReplace :: Test
runReplace = dbTestCase (\dbh ->
    do _ <- sRun dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r1
       _ <- sRun dbh "INSERT INTO hdbctest1 VALUES (?, ?, 2, ?)" r2
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = 'runReplace' ORDER BY testid"
       _ <- sExecute sth []
       sFetchRow sth >>= (assertEqual "r1" (Just r1))
       sFetchRow sth >>= (assertEqual "r2" (Just [Just "runReplace", Just "2",
                                                 Just "2", Nothing]))
       sFetchRow sth >>= (assertEqual "lastrow" Nothing)
                       )
    where r1 = [Just "runReplace", Just "1", Just "1234", Just "testdata"]
          r2 = [Just "runReplace", Just "2", Nothing]

executeReplace :: Test
executeReplace = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('executeReplace',?,?,?)"
       _ <- sExecute sth [Just "1", Just "1234", Just "Foo"]
       _ <- sExecute sth [Just "2", Nothing, Just "Bar"]
       commit dbh
       sth' <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = ? ORDER BY testid"
       _ <- sExecute sth' [Just "executeReplace"]
       sFetchRow sth' >>= (assertEqual "r1"
                         (Just $ map Just ["executeReplace", "1", "1234", 
                                           "Foo"]))
       sFetchRow sth' >>= (assertEqual "r2"
                         (Just [Just "executeReplace", Just "2", Nothing,
                                Just "Bar"]))
       sFetchRow sth' >>= (assertEqual "lastrow" Nothing)
                            )

testExecuteMany :: Test
testExecuteMany = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('multi',?,?,?)"
       sExecuteMany sth rows
       commit dbh
       sth' <- prepare dbh "SELECT testid, testint, testtext FROM hdbctest1 WHERE testname = 'multi' ORDER BY testid"
       _ <- sExecute sth' []
       mapM_ (\r -> sFetchRow sth' >>= (assertEqual "" (Just r))) rows
       sFetchRow sth >>= (assertEqual "lastrow" Nothing)
                          )
    where rows = [map Just ["1", "1234", "foo"],
                  map Just ["2", "1341", "bar"],
                  [Just "3", Nothing, Nothing]]

testsFetchAllRows :: Test
testsFetchAllRows = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('sFetchAllRows', ?, NULL, NULL)"
       sExecuteMany sth rows
       commit dbh
       sth' <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'sFetchAllRows' ORDER BY testid"
       _ <- sExecute sth' []
       results <- sFetchAllRows sth'
       assertEqual "" rows results
                               )
    where rows = map (\x -> [Just . show $ x]) ([1..9 :: Int])

basicTransactions :: Test
basicTransactions = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('basicTransactions', ?, NULL, NULL)"
       _ <- sExecute sth [Just "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'basicTransactions' ORDER BY testid"
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "initial commit" [[Just "0"]])

       -- Now try a rollback
       sExecuteMany sth rows
       rollback dbh
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "rollback" [[Just "0"]])

       -- Now try another commit
       sExecuteMany sth rows
       commit dbh
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "final commit" ([Just "0"]:rows))
                               )
    where rows = map (\x -> [Just . show $ x]) ([1..9 :: Int])

testWithTransaction :: Test
testWithTransaction = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('withTransaction', ?, NULL, NULL)"
       _ <- sExecute sth [Just "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'withTransaction' ORDER BY testid"
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "initial commit" [[Just "0"]])
       
       -- Let's try a rollback.
       catch (withTransaction dbh (\_ -> do sExecuteMany sth rows
                                            fail "Foo"))
             ( (\_ -> return ()) :: SomeException -> IO () )
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "rollback" [[Just "0"]])

       -- And now a commit.
       withTransaction dbh (\_ -> sExecuteMany sth rows)
       _ <- sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "final commit" ([Just "0"]:rows))
                               )
    where rows = map (\x -> [Just . show $ x]) ([1..9 :: Int])
       
tests :: Test
tests = TestList
        [
         TestLabel "openClosedb" openClosedb,
         TestLabel "multiFinish" multiFinish,
         TestLabel "runRawTest" runRawTest,
         TestLabel "runRawErrorTest" runRawErrorTest,
         TestLabel "basicQueries" basicQueries,
         TestLabel "createTable" createTable,
         TestLabel "runReplace" runReplace,
         TestLabel "executeReplace" executeReplace,
         TestLabel "executeMany" testExecuteMany,
         TestLabel "sFetchAllRows" testsFetchAllRows,
         TestLabel "basicTransactions" basicTransactions,
         TestLabel "withTransaction" testWithTransaction,
         TestLabel "dropTable" dropTable
         ]
