{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module TestTime(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Convertible
import SpecificDB

newtype ZonedTimeEq = ZonedTimeEq { _zt :: ZonedTime }

instance Show ZonedTimeEq where
    show = show . _zt

instance Eq ZonedTimeEq where
    a == b = let a' = _zt a
                 b' = _zt b
              in zonedTimeToUTC a' == zonedTimeToUTC b' &&
                 zonedTimeZone a' == zonedTimeZone b'

instance (Convertible a ZonedTime) => (Convertible a ZonedTimeEq) where
    safeConvert v = ZonedTimeEq <$> (safeConvert v)
instance (Convertible ZonedTime b) => (Convertible ZonedTimeEq b) where
    safeConvert (ZonedTimeEq v) = safeConvert v

testZonedTime :: ZonedTimeEq
testZonedTime = ZonedTimeEq . fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                 "1989-08-01T15:33:01 -0500"

testZonedTimeFrac :: ZonedTimeEq
testZonedTimeFrac = ZonedTimeEq . fromJust $ parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    "1989-08-01T15:33:01.536 -0500"

testDTType :: forall a. (Eq a, Show a, Convertible SqlValue a) =>
              a -> (a -> SqlValue) -> Test
testDTType inputdata convToSqlValue = dbTestCase $ \dbh ->
    do runRaw dbh ("CREATE TABLE hdbctesttime (testid INTEGER PRIMARY KEY NOT NULL, \
                \testvalue " ++ dateTimeTypeOfSqlValue value ++ ")")
       finally (convcmp dbh) (do commit dbh
                                 runRaw dbh "DROP TABLE hdbctesttime"
                                 commit dbh
                             )
    where convcmp dbh =
              do _ <- run dbh "INSERT INTO hdbctesttime (testid, testvalue) VALUES (?, ?)"
                      [iToSql 5, value]
                 commit dbh
                 r <- quickQuery' dbh "SELECT testid, testvalue FROM hdbctesttime" []
                 case r of
                   [[testidsv, testvaluesv]] -> 
                       do assertEqual "testid" (5::Int) (fromSql testidsv)
                          assertEqual "testvalue" inputdata (fromSql testvaluesv)
                   _ -> assertEqual "testquery" "one pair" "not one pair"
          value = convToSqlValue inputdata

mkTest :: forall a. (Eq a, Show a, Convertible SqlValue a) =>
           String -> a -> (a -> SqlValue) -> Test
mkTest label inputdata convfunc =
    TestLabel label (testDTType inputdata convfunc)

tests :: Test
tests = TestList $
    ((TestLabel "Non-frac" $ testIt testZonedTime) :
     if supportsFracTime then [TestLabel "Frac" $ testIt testZonedTimeFrac] else [])

testIt :: ZonedTimeEq -> Test
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
      baseZonedTimeOfDay = fromSql (SqlZonedTime $ _zt baseZonedTime)

      baseLocalTime :: LocalTime
      baseLocalTime = zonedTimeToLocalTime $ _zt baseZonedTime

      baseUTCTime :: UTCTime
      baseUTCTime = convert baseZonedTime

      basePOSIXTime :: POSIXTime
      basePOSIXTime = convert baseZonedTime

      baseDiffTime :: NominalDiffTime
      baseDiffTime = basePOSIXTime

      baseTimeDiff :: DiffTime
      baseTimeDiff = secondsToDiffTime 1506226306
