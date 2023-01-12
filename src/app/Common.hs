module Common where

import System.IO
import Control.Monad.Random

invalidCommand = "\nInvalid command, try again!"

wordsFileName = "valid-wordle-words-new.txt"

-- easyGameDifficulty = "Easy"
-- normalGameDifficulty = "Normal"
-- expertGameDifficulty = "Expert"

wordsLength :: Int
wordsLength = 5

easyDifficulty = "Easy"
normalDifficulty = "Normal"
expertDifficulty = "Expert"

data GameDifficulty = EasyGame | NormalGame | ExpertGame deriving (Enum, Eq)

data LetterStatus = GREY | YELLOW | GREEN deriving (Enum, Eq, Show)

allStatuses = [GREY, YELLOW, GREEN]

type LetterInfo = (Char, LetterStatus, Int)

type UserGuess = (String, [LetterInfo])

putStrAndFlush :: String -> IO ()
putStrAndFlush str = do
    putStr str
    hFlush stdout

getGuessResultAsString :: UserGuess -> String
getGuessResultAsString (_, letters) = 
    let lettersToString = foldr 
            (\(l, s, _) acc -> l:(concat $ replicate (length (show s) - 1) " ") ++ " | " ++ acc) "" letters
        resultsToString = foldr (\(_, s, _) acc -> (show s) ++ " | " ++ acc) "" letters 
    in  lettersToString ++ "\n" ++ resultsToString 

evaluateGuessResult :: String -> String -> UserGuess
evaluateGuessResult target guess = 
    let resultsFromGuess :: [LetterInfo]
        resultsFromGuess = foldr 
                (\x acc -> 
                    let targetChar = target !! x
                        guessChar = guess !! x
                    in  if guessChar == targetChar
                            then (guessChar, GREEN, x):acc
                        else if any (\y -> y == guessChar) target 
                            then (guessChar, YELLOW, x):acc
                        else (guessChar, GREY, x):acc) [] [0..(length target - 1)] 
    in  (guess, resultsFromGuess)

-- getAllCombinations :: [[a]] -> [[Maybe a]] 
-- getAllCombinations lst =
--     let replaceEmpty :: [a] -> [Maybe a]
--         replaceEmpty lst = if null lst then [Nothing] else map Just lst 
        
--         maybeList = foldr (\x acc -> if null x then [Nothing]:acc else map Just x:acc) [] lst

--         getAllCombinationsReversed :: [[Maybe a]] -> [Maybe a] -> [[Maybe a]] 
--         getAllCombinationsReversed (h:t) acc
--             | not $ null t = concatMap (\x -> getAllCombinationsReversed t (x:acc)) h
--             | otherwise = map (\x -> (x:acc)) h

--     in  map reverse $ getAllCombinationsReversed maybeList [] 