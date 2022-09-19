import Data.List
import System.IO 
import System.Environment 
import Control.Monad
import ColoredOutput
import qualified Dates
import qualified Transactions

dataFile :: String
dataFile = "data-test.txt"

main :: IO ()
main = do
    args <- getArgs
    infoMessage $ "Provided arguments: " ++ show args
    handleActions args

handleActions :: [String] -> IO ()
handleActions [] = infoMessage "Run ./main with as many as you wish of the following arguments: [spend, receive, history]"
handleActions (arg:rest) = do
    if arg == "spend"
        then spend
    else if arg == "receive"
        then receive
    else if arg == "history"
        then showHistory
    else warningMessage $ "Oops, " ++ arg ++ " is not a known argument. Please try 'spend', 'receive' or 'history'"
    handleActions rest

spend :: IO ()
spend = do
    transaction <- Transactions.askExpense
    Transactions.appendTransaction dataFile transaction

receive :: IO ()
receive = do
    transaction <- Transactions.askIncome
    Transactions.appendTransaction dataFile transaction

showHistory :: IO ()
showHistory = do
    infoMessage "\n ===== Transaction History ===== \n"
    contents <- readFile dataFile
    let ls = lines contents
    mapM showBudgetLine ls
    infoMessage "\n ===== End Of Transaction History ===== \n"
    return ()

showBudgetLine :: String -> IO ()
showBudgetLine line = do
    let amount = words line !! 1
    if (read amount < 0) then (warningMessage line) else (successMessage line)

