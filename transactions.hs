module Transactions (Transaction, appendTransaction, askExpense, askIncome, printTransaction, printTransactionsHistory) where

import Data.List
import Data.Maybe
import qualified Dates
import ColoredOutput

data Transaction = Transaction Dates.Date Amount Description deriving (Show, Read)
type Amount = Float
type Description = String

-- (date, amount, desc) --> "date amount desc"
tr2str :: Transaction -> String
tr2str (Transaction date amount desc) = (Dates.date2str date) ++ " " ++ (show amount) ++ " " ++ desc ++ "\n"

appendTransaction :: String -> Transaction -> IO ()
appendTransaction file tr = appendFile file $ show tr ++ "\n"

inverseTransaction :: Transaction -> Transaction
inverseTransaction (Transaction date a desc) = Transaction date (-a) desc

askExpense :: IO Transaction 
askExpense = do
    warningMessage "\nAmount spent: \n"
    amount <- getLine
    neutralMessage "Description: \n"
    desc <- getLine
    currentDate <- Dates.date
    return (Transaction currentDate (-read amount) desc)

askIncome :: IO Transaction
askIncome = do
    successMessage "\nAmount received: \n"
    amount <- getLine
    neutralMessage "Description: \n"
    desc <- getLine
    currentDate <- Dates.date
    return (Transaction currentDate (read amount) desc)

printTransaction :: Transaction -> IO ()
printTransaction tr@(Transaction date amount desc) = 
    if (amount < 0) 
    then (warningMessage $ tr2str tr) 
    else if (amount > 0) then (successMessage $ tr2str tr)
    else (neutralMessage $ tr2str tr)

printTransactionsHistory :: String -> IO ()
printTransactionsHistory file = do
    infoMessage "\n ===== Transaction History ===== \n\n"
    contents <- readFile file
    let ls = lines contents
    mapM printTransaction $ (map read ls :: [Transaction])
    infoMessage "\n ===== End Of Transaction History ===== \n\n"
    return ()