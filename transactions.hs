module Transactions (
    Transaction, 
    appendTransaction,
    askExpense, 
    askIncome, 
    printTransaction, 
    printTransactionsHistory,
    readTransactions
) where

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

readTransactions :: String -> IO [Transaction]
readTransactions file = do 
    contents <- readFile file
    let ls = lines contents
    return $ (map read ls :: [Transaction])

printTransaction :: Transaction -> IO ()
printTransaction tr@(Transaction date amount desc) = 
    if (amount < 0) 
    then (warningMessage $ tr2str tr) 
    else if (amount > 0) then (successMessage $ tr2str tr)
    else (neutralMessage $ tr2str tr)

printTransactionsHistory :: [Transaction] -> IO ()
printTransactionsHistory transactions = do
    infoMessage "\n ===== Transaction History ===== \n\n"
    mapM printTransaction $ transactions
    infoMessage "\n ===== End Of Transaction History ===== \n\n"
    return ()

transactionsBill :: [Transaction] -> Amount
transactionsBill = foldr (\(Transaction _ a _) sum -> sum + a) 0