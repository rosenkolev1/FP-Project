module Main where

import System.IO
import Control.Monad.Random
import Data.Semigroup (diff)
import Data.Char (chr)
import Text.Read (Lexeme(String))
import Data.Maybe (fromJust, isJust) 
import GameMode (startGameMode)
import ProgramMode (startProgramMode)
import Common

gameMode = "Game"
helperMode = "Helper"

main = do  
    putStrLn $ "Hello, what mode would you like to start the program in: (" ++ gameMode ++ "/" ++ helperMode ++ ")" 
    programMode <- getLine

    if programMode == gameMode
        then do
            putStrLn "\nProgram is in game mode!" 
            startGameMode
    else if programMode == helperMode
        then do 
            putStrLn "\nThe program is in helper mode!"
            startProgramMode
    else do 
        putStrLn invalidCommand
        main
