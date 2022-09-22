module ColoredOutput (
    successMessage,
    warningMessage,
    neutralMessage,
    infoMessage,
    smartMessage
) where

import qualified Utils

data Color = Default | Red | Green | Cyan | Yellow

-- https://stackoverflow.com/questions/21220142/change-color-of-a-string-haskell

color2ansiCode :: Color -> String
color2ansiCode Default = "\ESC[0m"
color2ansiCode Red = "\ESC[31m"
color2ansiCode Green = "\ESC[32m"
color2ansiCode Cyan = "\ESC[36m"
color2ansiCode Yellow = "\ESC[93m"

changePrintColor :: Color -> IO ()
changePrintColor c = do
    putStr $ color2ansiCode c
    return ()

putStrColor :: Color -> String -> IO ()
putStrColor c message = do
    changePrintColor c
    putStr message
    changePrintColor Default

warningMessage :: String -> IO ()
warningMessage = putStrColor Red

successMessage :: String -> IO ()
successMessage = putStrColor Green

neutralMessage :: String -> IO ()
neutralMessage = putStrColor Cyan

infoMessage :: String -> IO ()
infoMessage = putStrColor Yellow

-- changes the color depending on whether the 1st arg is within a given range (3rd arg) of the 2nd arg
smartMessage :: Float -> Float -> Float -> String -> IO()
smartMessage val target range = if Utils.inCenteredInterval val target range
                                    then neutralMessage
                                else if val < target - range
                                    then warningMessage
                                else successMessage