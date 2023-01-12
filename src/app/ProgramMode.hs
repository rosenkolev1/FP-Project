module ProgramMode where

import System.IO
import Control.Monad.Random
import Data.Semigroup (diff)
import Data.Char (chr)
import Text.Read (Lexeme(String))
import Data.Maybe (fromJust, isJust)
import Data.List.Split (splitOn)
import Common

data ProgramDifficulty =  NormalProgram | ExpertProgram deriving (Enum, Eq)

startProgramMode :: IO()
startProgramMode = do
    putStrLn $ "\nWhich difficulty would you like to help in: (" ++ normalDifficulty ++ "/" ++ expertDifficulty ++ ")"
    programDifficulty <- getLine

    if programDifficulty == normalDifficulty
        then do
            putStrLn "\nThe program is on normal difficulty!"

            content <- readFile wordsFileName

            programInputGuess [] 1 NormalProgram (lines content)

    else if programDifficulty == expertDifficulty
        then do 
            putStrLn "\nThe program is on expert difficulty!"

            content <- readFile wordsFileName

            programInputGuess [] 1 ExpertProgram (lines content)

    else do
        putStrLn invalidCommand
        startProgramMode

waitForUserAnswer :: [UserGuess] -> Int -> ProgramDifficulty -> [String] -> String -> IO ()
waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selected = do
    putStrLn $ "\nGuess " ++ show curGuessNumber ++ ": " ++ selected ++ " | now show me the evaluation result! (split by whitespaces)"
        ++ " (GREY/YELLOW,GREEN) (requires all 5)"
    input <- getLine

    let splitInput = words input

        inputIsInvalid = length splitInput /= 5 ||
            any (\x -> x /= show GREY && x /= show GREEN && x /= show YELLOW) splitInput

    if inputIsInvalid 
        then do
            putStrLn invalidCommand
            waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selected
    else do

        let statuses = map (\x -> if x == show GREY then GREY else if x == show GREEN then GREEN else YELLOW) splitInput
            hasGuessedCorrectly = all (== GREEN) statuses
            
        if hasGuessedCorrectly
            then do
                putStrLn $ "I have guessed correctly! GET FUCKED BITCH!"
        else do
            let newGuessLettersInfo = zip3 selected statuses [0..]
                newGuess :: UserGuess
                newGuess = (selected, newGuessLettersInfo)

                newPossibleTargets = filter (\target -> newGuess == evaluateGuessResult target selected) possibleTargets

                newGuesses = newGuess:guesses

            --DEBUG
            putStrLn $ show newPossibleTargets

            programInputGuess newGuesses (curGuessNumber + 1) difficulty newPossibleTargets


programInputGuess :: [UserGuess] -> Int -> ProgramDifficulty -> [String] -> IO ()
programInputGuess guesses curGuessNumber difficulty possibleTargets = do
    -- putStrAndFlush "Input the word that you wish the computer to guess: "

    --Get all the targetWords which would return the same old guesses
    -- let allPossibleTargets :: [UserGuess] -> [String]
    --     allPossibleTargets oldGuesses = 
    --         foldr (\newTargetWord acc -> 
    --             let evalResults = foldr (\(guessWord, _) acc2 -> (evaluateGuessResult newTargetWord guessWord):acc2) [] oldGuesses
    --             in  if evalResults == oldGuesses then newTargetWord:acc else acc) 
    --                 [] possibleTargets

    if null possibleTargets
        then do
            putStrLn "The inputs that you have given me are incorrect.\nNo word matches the given inputs. Aborting program!"
    else if curGuessNumber > 6
        then do
            putStrLn "I couldn't guess the word! Fuck you!"
    else if length possibleTargets == 1
        then do
            putStrLn $ "The only possible word remaining is: " ++ head possibleTargets ++ 
                       "\nIf this isn't the correct word, then you are an idiot sir.\nGoodbye!"
    else if curGuessNumber == 1
        then do
            let selectedGuess = "pilot"           
            waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selectedGuess
    else do
        let 
            getCountOfRemovedForGuess :: String -> Int
            getCountOfRemovedForGuess word = 
                foldr (\potentialTarget acc2 -> 
                    let guessEval = evaluateGuessResult word potentialTarget
                        newPossibleTargets = filter (\oldTarget -> guessEval == evaluateGuessResult oldTarget potentialTarget) possibleTargets
                    in  (length possibleTargets - length newPossibleTargets) + acc2) 0 possibleTargets

        -- forM_ (zip possibleTargets [1..]) (\(word, index) -> do
        --     putStrLn $ show index ++ ". " ++ (show $ getCountOfRemovedForGuess word))

        let guessesWithRemoved :: [(String, Int)]
            guessesWithRemoved = map (\word -> (word, getCountOfRemovedForGuess word)) possibleTargets

            maxRemovedCount = foldr 
                (\(word, removedCount) acc -> max removedCount acc) (-1) guessesWithRemoved

            guessesWithRemovedMax = filter (\(word, removedCount) -> removedCount == maxRemovedCount) guessesWithRemoved

        randomNumber <- evalRandIO $ getRandomR (0, length guessesWithRemovedMax - 1)

        let selectedGuess = fst (guessesWithRemovedMax !! randomNumber)     

        waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selectedGuess
