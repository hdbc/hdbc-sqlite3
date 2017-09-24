{-# LANGUAGE FlexibleContexts #-}

module TestTime(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import Control.Exception
import Data.Time
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Convertible
import SpecificDB

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b &&
             zonedTimeZone a == zonedTimeZone b

testZonedTime :: ZonedTime
testZonedTime = fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                 "1989-08-01T15:33:01 -0500"

testZonedTimeFrac :: ZonedTime
testZonedTimeFrac = fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    "1989-08-01T15:33:01.536 -0500"


rowdata t = [[SqlInt32 100, toSql t, SqlNull]]

testDTType inputdata convToSqlValue = dbTestCase $ \dbh ->
    do runRaw dbh ("CREATE TABLE hdbctesttime (testid INTEGER PRIMARY KEY NOT NULL, \
                \testvalue " ++ dateTimeTypeOfSqlValue value ++ ")")
       finally (testIt dbh) (do commit dbh
                                runRaw dbh "DROP TABLE hdbctesttime"
                                commit dbh
                            )
    where testIt dbh =
              do run dbh "INSERT INTO hdbctesttime (testid, testvalue) VALUES (?, ?)"
                     [iToSql 5, value]
                 commit dbh
                 r <- quickQuery' dbh "SELECT testid, testvalue FROM hdbctesttime" []
                 case r of
                   [[testidsv, testvaluesv]] -> 
                       do assertEqual "testid" (5::Int) (fromSql testidsv)
                          assertEqual "testvalue" inputdata (fromSql testvaluesv)
          value = convToSqlValue inputdata

mkTest label inputdata convfunc =
    TestLabel label (testDTType inputdata convfunc)

tests = TestList $
    ((TestLabel "Non-frac" $ testIt testZonedTime) :
     if supportsFracTime then [TestLabel "Frac" $ testIt testZonedTimeFrac] else [])

testIt baseZonedTime = 
    TestList [mkTest "Day" baseDay toSql,
              mkTest "TimeOfDay" baseTimeOfDay toSql,
              mkTest "ZonedTimeOfDay" baseZonedTimeOfDay toSql,
              mkTest "LocalTime" baseLocalTime toSql,
              mkTest "ZonedTime" baseZonedTime toSql,
              mkTest "UTCTime" baseUTCTime toSql,
              mkTest "DiffTime" baseDiffTime toSql,
              mkTest "POSIXTime" basePOSIXTime posixToSql,
              mkTest "TimeDiff" baseTimeDiff toSql
             ]
    where 
      baseDay :: Day
      baseDay = localDay baseLocalTime

      baseTimeOfDay :: TimeOfDay
      baseTimeOfDay = localTimeOfDay baseLocalTime

      baseZonedTimeOfDay :: (TimeOfDay, TimeZone)
      baseZonedTimeOfDay = fromSql (SqlZonedTime baseZonedTime)

      baseLocalTime :: LocalTime
      baseLocalTime = zonedTimeToLocalTime baseZonedTime

      baseUTCTime :: UTCTime
      baseUTCTime = convert baseZonedTime

      basePOSIXTime :: POSIXTime
      basePOSIXTime = convert baseZonedTime

      baseDiffTime :: NominalDiffTime
      baseDiffTime = basePOSIXTime

      baseTimeDiff :: DiffTime
      baseTimeDiff = secondsToDiffTime 1506226306
