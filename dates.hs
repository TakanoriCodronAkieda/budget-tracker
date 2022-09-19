module Dates (
    Date,
    date, 
    date2str,
    str2date
) where

import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import qualified Utils

type Date = (Integer, Int, Int)

date :: IO Date -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

-- (y, m, d) -> "y/m/d"
date2str :: Date -> String
date2str (year, month, day) = intercalate "/" $ map show [fromIntegral year, month, day]

-- "y/m/d" --> (y, m, d) 
str2date :: String -> Maybe Date
str2date s = case Utils.splitOn "/" s of 
                                    [y, m, d] -> Just (read y, read m, read d)
                                    _ -> Nothing


