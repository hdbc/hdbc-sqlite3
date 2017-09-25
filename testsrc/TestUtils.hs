module TestUtils(connectDB, connectDBExt, sqlTestCase, dbTestCase, dbTestCaseExt, printDBInfo) where
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.HUnit
import Control.Exception
import SpecificDB(connectDB, connectDBExt)

sqlTestCase :: IO () -> Test
sqlTestCase a = 
    TestCase (handleSqlError a)

dbTestCase :: (Connection -> IO ()) -> Test
dbTestCase a =
    TestCase (do dbh <- connectDB
                 finally (handleSqlError (a dbh))
                         (handleSqlError (disconnect dbh))
             )

dbTestCaseExt :: Bool -> (Connection -> IO()) -> Test
dbTestCaseExt auto a =
    TestCase (do dbh <- connectDBExt auto
                 finally (handleSqlError (a dbh))
                         (handleSqlError (disconnect dbh))
             )

printDBInfo :: IO ()
printDBInfo = handleSqlError $
    do dbh <- connectDB
       putStrLn "+-------------------------------------------------------------------------"
       putStrLn $ "| Testing HDBC database module: " ++ hdbcDriverName dbh ++
                ", bound to client: " ++ hdbcClientVer dbh
       putStrLn $ "| Proxied driver: " ++ proxiedClientName dbh ++
                ", bound to version: " ++ proxiedClientVer dbh
       putStrLn $ "| Connected to server version: " ++ dbServerVer dbh
       putStrLn "+-------------------------------------------------------------------------\n"
       disconnect dbh
