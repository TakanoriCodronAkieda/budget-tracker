module Dates (
    Date,
    date, 
    date2str,
) where

import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import qualified Utils

type Date = (Integer, Int, Int)

-- https://stackoverflow.com/questions/52490035/how-to-get-date-in-haskell
date :: IO Date -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

-- (y, m, d) -> "y/m/d"
date2str :: Date -> String
date2str (year, month, day) = intercalate "/" $ map show [fromIntegral year, month, day]


-- -- very naive implementation, does not account for leap years and such
-- dayNumber :: Date -> Int
-- dayNumber (year, month, day) = 365 * year +

-- -- very naive and does not account for leap years and such
-- weeksDifference :: Date -> Date -> Int

-- weekNumber :: Date -> Int

