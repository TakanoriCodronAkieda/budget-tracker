module Utils (splitOn) where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn separator message = map T.unpack $ T.splitOn (T.pack separator) (T.pack message)