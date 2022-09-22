module Utils (splitOn, inCenteredInterval) where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn separator message = map T.unpack $ T.splitOn (T.pack separator) (T.pack message)

inCenteredInterval :: Float -> Float -> Float -> Bool
inCenteredInterval val target range = val >= target - range && val <= target + range