-- import System.IO
-- import Control.Monad.Random

-- wordsFileName = "valid-wordle-words.txt"

-- gameMode = "Game"
-- helperMode = "Helper"

-- easyGameDifficulty = "Easy"
-- normalGameDifficulty = "Normal"
-- expertGameDifficulty = "Expert"

-- invalidCommand = "\nInvalid command, try again!"
-- wordsLength = 5

-- putStrAndFlush :: String -> IO ()
-- putStrAndFlush str = do
--     putStr str
--     hFlush stdout

-- main = do  
--     putStrLn $ "Hello, what mode would you like to start the program in: (" ++ gameMode ++ "/" ++ helperMode ++ ")" 
--     programMode <- getLine

--     if programMode == gameMode
--         then do
--             putStrLn "\nProgram is in game mode!" 
--             startGameMode
--     else if programMode == helperMode
--         then putStrLn "The program is in helper mode!"
--     else do 
--         putStrLn invalidCommand
--         main

-- startGameMode :: IO ()
-- startGameMode = do
--     putStrLn $ "\nWhich difficulty would you like to play in: " ++ easyGameDifficulty
--         ++ "/" ++ normalGameDifficulty ++ "/" ++ expertGameDifficulty ++ ")"
--     gameDifficulty <- getLine

--     if gameDifficulty == easyGameDifficulty
--         then putStrLn "Not implemented yet"

--     else if gameDifficulty == normalGameDifficulty
--         then do 
--             putStrLn "\nThe game is on normal difficulty!"

--             --Get a random word from the text file
--             targetWord <- getRandomWord

--             userInputGuess targetWord []

--     else if gameDifficulty == expertGameDifficulty 
--         then putStrLn "Not implemented yet"

--     else do
--         putStrLn invalidCommand
--         startGameMode

-- userInputGuess :: String -> [String] -> IO ()
-- userInputGuess targetWord guesses = do
--     let curGuessNumber = length guesses + 1

--     if curGuessNumber > 6
--         then do
--             putStrLn "\nYou have lost the game! Sucks to be you loser!"
--             return ()
--     else do
--         putStrAndFlush $ "\nRemaining Guesses " ++ show (7 - curGuessNumber) ++ "\nGuess " ++ show curGuessNumber ++ ": "

--         userGuess <- getLine

--         if userGuess == targetWord
--             then do
--                 putStrLn "\nYou have guessed the word correctly! Congrats!"
--                 return ()
--         else if length userGuess /= wordsLength
--             then do
--                 putStrLn invalidCommand
--                 userInputGuess targetWord guesses
--         else do


--             putStrLn "\nThe word is incorrect, try again!"
--             userInputGuess targetWord (userGuess:guesses)

-- getRandomWord :: IO String
-- getRandomWord = do
--     contents <- readFile wordsFileName
--     return (head $ lines contents)

-- -- hFlush stdout 