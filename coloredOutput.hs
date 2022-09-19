module ColoredOutput (
    successMessage,
    warningMessage,
    neutralMessage,
    infoMessage
) where

data Color = Default | Red | Green | Cyan | Yellow

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