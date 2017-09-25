module SpecificDB where
import Database.HDBC
import Database.HDBC.Sqlite3

connectDB :: IO Connection
connectDB = 
    handleSqlError (connectSqlite3 "testtmp.sql3")

connectDBExt :: Bool -> IO Connection
connectDBExt auto =
    handleSqlError (connectSqlite3Ext auto False "testtmp.sql3")

dateTimeTypeOfSqlValue :: SqlValue -> String
dateTimeTypeOfSqlValue (SqlPOSIXTime _) = "TEXT"
dateTimeTypeOfSqlValue (SqlEpochTime _) = "INTEGER"
dateTimeTypeOfSqlValue _ = "TEXT"

supportsFracTime :: Bool
supportsFracTime = True
