module Transactions (Transaction, appendTransaction, askExpense, askIncome, printTransaction) where

import Data.List
import Data.Maybe
import qualified Dates
import ColoredOutput

type Transaction = (Dates.Date, Amount, Description)
type Amount = Int
type Description = String

-- (date, amount, desc) --> "date amount desc"
tr2str :: Transaction -> String
tr2str (date, amount, desc) = (Dates.date2str date) ++ " " ++ (show amount) ++ " " ++ desc ++ "\n"

appendTransaction :: String -> Transaction -> IO ()
appendTransaction file tr = appendFile file $ tr2str tr

inverseTransaction :: Transaction -> Transaction
inverseTransaction (date, a, desc) = (date, -a, desc)

askExpense :: IO Transaction 
askExpense = do
    warningMessage "Amount spent: "
    amount <- getLine
    neutralMessage "Description: "
    desc <- getLine
    currentDate <- Dates.date
    return (currentDate, -(read amount), desc)

askIncome :: IO Transaction
askIncome = do
    successMessage "Amount received: "
    amount <- getLine
    neutralMessage "Description: "
    desc <- getLine
    currentDate <- Dates.date
    return (currentDate, read amount, desc)

printTransaction :: Transaction -> IO ()
printTransaction tr@(date, amount, desc) = 
    if (amount < 0) 
    then (warningMessage $ tr2str tr) 
    else if (amount > 0) then (successMessage $ tr2str tr)
    else (neutralMessage $ tr2str tr)