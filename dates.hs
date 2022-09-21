module Dates (
    Date,
    date, 
    date2str,
    sameWeekNumber,
    weekNumber
) where

import Data.List
import Data.Time.Clock
import qualified Data.Time.Calendar as T
import qualified Utils

type Date = (Integer, Int, Int)

referenceDate :: Date -- the date when week # 0 starts
referenceDate = (2022, 9, 12)

-- https://stackoverflow.com/questions/52490035/how-to-get-date-in-haskell
date :: IO Date -- :: (year,month,day)
date = getCurrentTime >>= return . T.toGregorian . utctDay

-- (y, m, d) -> "y/m/d"
date2str :: Date -> String
date2str (year, month, day) = intercalate "/" $ map show [fromIntegral year, month, day]

diffDays :: Date -> Date -> Integer
diffDays (y1, m1, d1) (y2, m2, d2) = T.diffDays (T.fromGregorian y1 m1 d1) (T.fromGregorian y2 m2 d2)

diffWeeks :: Date -> Date -> Integer
diffWeeks a b = (diffDays a b) `div` 7 

weekNumber :: Date -> Integer
weekNumber a = diffWeeks a referenceDate

sameWeekNumber :: Date -> Date -> Bool
sameWeekNumber a b = weekNumber a == weekNumber b