module ProgramMode where

import System.IO
import Control.Monad.Random
import Data.Semigroup (diff)
import Data.Char (chr)
import Text.Read (Lexeme(String))
import Data.Maybe (fromJust, isJust)
import Data.List.Split (splitOn)
import Data.List (nub)
import Common

data ProgramDifficulty =  NormalProgram | ExpertProgram deriving (Enum, Eq)

startProgramMode :: IO()
startProgramMode = do
    putStrLn $ "\nWhich difficulty would you like to help in: (" ++ normalDifficulty ++ "/" ++ expertDifficulty ++ ")"
    programDifficulty <- getLine

    if programDifficulty == normalDifficulty
        then do
            putStrLn "\nThe program is on normal difficulty!"

            content <- readFile wordsFileNameHelperMode

            programInputGuess [] 1 NormalProgram (lines content) $ -1

    else if programDifficulty == expertDifficulty
        then do 
            putStrLn "\nThe program is on expert difficulty!"

            content <- readFile wordsFileNameHelperMode

            programInputGuess [] 1 ExpertProgram (lines content) $ -1

    else do
        putStrLn invalidCommand
        startProgramMode

programInputGuess :: [GuessResult] -> Int -> ProgramDifficulty -> [String] -> Int -> IO ()
programInputGuess guesses curGuessNumber difficulty possibleTargets maxLieIndex = do

    let considerLies = maxLieIndex >= 0 

    if null possibleTargets && (considerLies && difficulty == ExpertProgram || difficulty /= ExpertProgram)
        then do
            putStrLn "\nThe inputs that you have given me are incorrect.\nNo word matches the given inputs. Aborting program!"
    else if null possibleTargets && not considerLies && difficulty == ExpertProgram
        then programInputGuess guesses curGuessNumber difficulty ["meaningless"] (curGuessNumber - 2)
    else if curGuessNumber > 6
        then do
            putStrLn "\nI couldn't guess the word! Fuck you!"
    else if curGuessNumber == 1
        then do
            let selectedGuess = "pilot"           
            waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selectedGuess maxLieIndex
    else do
        let 
            getCountOfRemovedForGuess :: String -> [String] -> Int
            getCountOfRemovedForGuess word targets = 
                foldr (\potentialTarget acc2 -> 
                    let guessEval = evaluateGuessResult word potentialTarget
                        newPossibleTargets = filter (\oldTarget -> guessEval == evaluateGuessResult oldTarget potentialTarget) targets
                    in  (length targets - length newPossibleTargets) + acc2) 0 targets

            guessesWithRemoved :: [String] -> [(String, Int)]
            guessesWithRemoved targets = map (\word -> (word, getCountOfRemovedForGuess word targets)) targets

        if difficulty == NormalProgram || (difficulty == ExpertProgram && not considerLies)
            then do
                let currentTargets = guessesWithRemoved possibleTargets 
                
                selectedGuess <- getSelectedGuess currentTargets   

                waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selectedGuess maxLieIndex
        else do 
            content <- readFile wordsFileNameHelperMode
            --Get all the targetWords which would return the same old guesses
            let allLines = lines content

                allPossibleTargets :: [GuessResult] -> [String]
                allPossibleTargets oldGuesses = 
                    foldr (\newTargetWord acc -> 
                        let evalResults = foldr (\(guessWord, _) acc2 -> (evaluateGuessResult newTargetWord guessWord):acc2) [] oldGuesses
                        in  if evalResults == oldGuesses then newTargetWord:acc else acc) 
                            [] allLines
                
                allPossibleGuesses :: [[GuessResult]]
                allPossibleGuesses = (map (\i ->
                    let possibleGuess = take i guesses ++ drop (i + 1) guesses
                    in  possibleGuess) [0..maxLieIndex])

            --DEBUG
            forM_ allPossibleGuesses (\x -> putStrLn $ "Truthful guesses:\n" ++ show x ++ "\n")
            putStrLn $ "All guesses: " ++ show guesses

            let   
                allTargetsWithRemovedWithDuplicates = 
                    concatMap (\guesses -> guessesWithRemoved $ allPossibleTargets guesses) allPossibleGuesses

                allUniqueTargets = nub $ map (\(target,_) -> target) allTargetsWithRemovedWithDuplicates
                
                allTargetsWithRemoved = map (\target -> 
                    foldr (\(target2, removed) acc -> if target == target2 then (target, removed + snd acc) else acc) 
                        (target, 0) allTargetsWithRemovedWithDuplicates) allUniqueTargets

            forM_ allPossibleGuesses (\gs -> do
                putStrLn $ "\nFor the truthful guesses: " ++ show gs ++ "\nThe possible targets are:\n" ++ (show $ allPossibleTargets gs))

            putStrLn $ "All the possible targets with removed and duplicates for all lying possibilities: " ++ 
                show allTargetsWithRemovedWithDuplicates

            selectedGuess <- getSelectedGuess allTargetsWithRemoved     

            waitForUserAnswer guesses curGuessNumber difficulty allUniqueTargets selectedGuess maxLieIndex

getSelectedGuess :: [(String, Int)] -> IO String
getSelectedGuess allTargets = do
    let maxRemovedCount = foldr 
                (\(word, removedCount) acc -> max removedCount acc) (-1) allTargets

        guessesWithRemovedMax = filter (\(word, removedCount) -> removedCount == maxRemovedCount) allTargets

    randomNumber <- evalRandIO $ getRandomR (0, length guessesWithRemovedMax - 1)
    let selectedGuess = fst (guessesWithRemovedMax !! randomNumber)

    return selectedGuess

waitForUserAnswer :: [GuessResult] -> Int -> ProgramDifficulty -> [String] -> String -> Int -> IO ()
waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selected maxLieIndex = do
    putStrLn $ "\nGuess " ++ show curGuessNumber ++ ": " ++ selected ++ " | now show me the evaluation result! (split by whitespaces)"
        ++ " (GREY/YELLOW/GREEN) (requires all 5)"
    input <- getLine

    let splitInput = words input

        inputIsInvalid = length splitInput /= 5 ||
            any (\x -> x /= show GREY && x /= show GREEN && x /= show YELLOW) splitInput

    if inputIsInvalid 
        then do
            putStrLn invalidCommand
            waitForUserAnswer guesses curGuessNumber difficulty possibleTargets selected maxLieIndex
    else do

        let statuses = map (\x -> if x == show GREY then GREY else if x == show GREEN then GREEN else YELLOW) splitInput
            hasGuessedCorrectly = all (== GREEN) statuses
            
        if hasGuessedCorrectly
            then do
                putStrLn $ "\nI have guessed correctly! GET FUCKED BITCH!"
        else do
            let newGuessLettersInfo = zip3 selected statuses [0..]
                newGuess :: GuessResult
                newGuess = (selected, newGuessLettersInfo)

                newPossibleTargets = filter (\target -> newGuess == evaluateGuessResult target selected) possibleTargets

                newGuesses = guesses ++ [newGuess]

            --DEBUG --works only for normal mode
            putStrLn $ show newPossibleTargets

            programInputGuess newGuesses (curGuessNumber + 1) difficulty newPossibleTargets maxLieIndex


            