module Transactions (
    Transaction, 
    appendTransaction,
    askExpense, 
    askIncome, 
    printTransaction, 
    printTransactionsHistory,
    printTransactionsHistoryByWeeks,
    readTransactions
) where

import Data.List
import Data.Maybe
import qualified Dates
import ColoredOutput

data Transaction = Transaction Dates.Date Amount Description deriving (Show, Read)
type Amount = Float
type Description = String

weeklyLimit :: Float
weeklyLimit = -100

-- (date, amount, desc) --> "date amount desc"
tr2str :: Transaction -> String
tr2str (Transaction date amount desc) = (Dates.date2str date) ++ " " ++ (show amount) ++ " " ++ desc ++ "\n"

inverseTransaction :: Transaction -> Transaction
inverseTransaction (Transaction date a desc) = Transaction date (-a) desc

readTransactions :: String -> IO [Transaction]
readTransactions file = do 
    contents <- readFile file
    let ls = lines contents
    return $ (map read ls :: [Transaction])

transactionsBill :: [Transaction] -> Amount
transactionsBill = foldr (\(Transaction _ a _) sum -> sum + a) 0

groupTransactionsByWeek :: [Transaction] -> [[Transaction]]
groupTransactionsByWeek = groupBy sameWeek
    where sameWeek (Transaction d1 _ _) (Transaction d2 _ _) = Dates.sameWeekNumber d1 d2

appendTransaction :: String -> Transaction -> IO ()
appendTransaction file tr = appendFile file $ show tr ++ "\n"

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

printTransactionsHistory :: [Transaction] -> IO ()
printTransactionsHistory transactions = do
    mapM printTransaction $ transactions
    return ()

printTransactionWeek :: [Transaction] -> IO ()
printTransactionWeek [] = do
    infoMessage "\nNo transactions to show \n"
    return ()
printTransactionWeek ts@((Transaction date _ _):_) = do
    infoMessage $ "\nWeek " ++ (show $ Dates.weekNumber date) ++ "\n"
    printTransactionsHistory ts
    let total = transactionsBill ts
    infoMessage $ "Weekly spending: "
    smartMessage total weeklyLimit 5 $ show total ++ "\n"
    
printTransactionsHistoryByWeeks :: [Transaction] -> IO ()
printTransactionsHistoryByWeeks ts = do
    let total = transactionsBill ts
    let groups = groupTransactionsByWeek ts
    let nWeeks = fromIntegral $ length groups
    mapM printTransactionWeek groups
    infoMessage "\nAverage weekly expenses: "
    smartMessage total weeklyLimit 5 $ show (total / nWeeks) ++ "\n"
