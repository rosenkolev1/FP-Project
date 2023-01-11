module Main where

import System.IO
import Control.Monad.Random
import Data.Semigroup (diff)
import Data.Char (chr)
import Text.Read (Lexeme(String))
import Data.Maybe (fromJust, isJust)

wordsFileName = "valid-wordle-words.txt"

gameMode = "Game"
helperMode = "Helper"

easyGameDifficulty = "Easy"
normalGameDifficulty = "Normal"
expertGameDifficulty = "Expert"

invalidCommand = "\nInvalid command, try again!"
wordsLength = 5

data GameDifficulty = EasyGame | NormalGame | ExpertGame deriving (Enum, Eq)

data LetterStatus = GREY | YELLOW | GREEN deriving (Enum, Eq, Show)

allStatuses = [GREY, YELLOW, GREEN]

type LetterInfo = (Char, LetterStatus, Int)

type UserGuess = (String, [LetterInfo])

putStrAndFlush :: String -> IO ()
putStrAndFlush str = do
    putStr str
    hFlush stdout

main = do  
    putStrLn $ "Hello, what mode would you like to start the program in: (" ++ gameMode ++ "/" ++ helperMode ++ ")" 
    programMode <- getLine

    if programMode == gameMode
        then do
            putStrLn "\nProgram is in game mode!" 
            startGameMode
    else if programMode == helperMode
        then putStrLn "The program is in helper mode!"
    else do 
        putStrLn invalidCommand
        main

startGameMode :: IO ()
startGameMode = do
    putStrLn $ "\nWhich difficulty would you like to play in: " ++ easyGameDifficulty
        ++ "/" ++ normalGameDifficulty ++ "/" ++ expertGameDifficulty ++ ")"
    gameDifficulty <- getLine

    if gameDifficulty == easyGameDifficulty
        then do
            putStrLn "\nThe game is on easy difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 EasyGame

    else if gameDifficulty == normalGameDifficulty
        then do 
            putStrLn "\nThe game is on normal difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 NormalGame

    else if gameDifficulty == expertGameDifficulty 
        then do
            putStrLn "\nThe game is on EXPERT difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 ExpertGame

    else do
        putStrLn invalidCommand
        startGameMode

userInputGuess :: String -> [UserGuess] -> Int -> GameDifficulty -> IO ()
userInputGuess targetWord guesses curGuessNumber difficulty = do

    if curGuessNumber > 6
        then do
            putStrLn "\nYou have lost the game! Sucks to be you loser!"
            return ()
    else do
        putStrAndFlush $ "\nRemaining Guesses " ++ show (7 - curGuessNumber) ++ "\nGuess " ++ show curGuessNumber ++ ": "

        userGuess <- getLine
        contents <- readFile wordsFileName

        if userGuess == targetWord
            then do
                putStrLn "\nYou have guessed the word correctly! Congrats!"
                return ()
        else if length userGuess /= wordsLength
            then do
                putStrLn invalidCommand
                userInputGuess targetWord guesses curGuessNumber difficulty
        else do
            let dictionary = lines contents
                guessExistsInDictionary = wordExists userGuess dictionary

            --Check if the word doesn't exist and the game is in easy diff
            if difficulty == EasyGame && not guessExistsInDictionary
                then do
                    putStrLn "\nThe word doesn't exist in the dictionary!"
                    userInputGuess targetWord guesses curGuessNumber difficulty

            --Check if the word doesn't exist and the game is not in easy diff
            else if difficulty /= EasyGame && not guessExistsInDictionary
                then do
                    putStrLn "\nThe word doesn't exist in the dictionary! Taking away a guess hehe!"
                    userInputGuess targetWord guesses (curGuessNumber + 1) difficulty
            else do
                
                let wordChecks = [wordContainsUselessGreyLetters, 
                                  wordDoesNotContainNeededYellowLetters,
                                  wordDoesNotContainKnownGreenLetters]

                    wordCheckResults = map (\x -> x userGuess guesses) wordChecks
                    wordCheckResultsStrings = map (\(Just x) -> x) (filter (\x -> x /= Nothing) wordCheckResults)

                if difficulty == EasyGame && (not $ null wordCheckResultsStrings)
                    then do
                        let warningString = foldr (\x acc -> x ++ "\n" ++ acc) "\n" wordCheckResultsStrings
                        
                        putStrAndFlush warningString
                        putStrLn "Proceed with it anyway? (yes/no)"

                        userConfirmation <- getLine

                        if userConfirmation /= "yes"
                            then userInputGuess targetWord guesses curGuessNumber difficulty

                        else resolveUserGuess targetWord guesses curGuessNumber difficulty userGuess

                else if difficulty == ExpertGame 
                    then do 
                        resolveUserGuessInExpertMode targetWord guesses curGuessNumber difficulty userGuess        

                else resolveUserGuess targetWord guesses curGuessNumber difficulty userGuess

resolveUserGuessInExpertMode :: String -> [UserGuess] -> Int -> GameDifficulty -> String -> IO ()
resolveUserGuessInExpertMode targetWord guesses curGuessNumber difficulty userGuess = do
    let possibleCombinations :: Int
        possibleCombinations = (3 ^ wordsLength) - 1

    randomNumber <- evalRandIO $ getRandomR (0, possibleCombinations)

    let userGuessLettersWithPositions = zip userGuess [0..]

        --TODO Change this maybe?
        allCombinations = [[status1, status2, status3, status4, status5]|
            status1 <- allStatuses,
            status2 <- allStatuses,
            status3 <- allStatuses,
            status4 <- allStatuses,
            status5 <- allStatuses]

        curCombination = allCombinations !! randomNumber

        fakeUserGuessLettersInfo :: [LetterInfo]
        fakeUserGuessLettersInfo = zip3 userGuess curCombination [0..]

        fakeUserGuess :: UserGuess
        fakeUserGuess = (userGuess, fakeUserGuessLettersInfo) 

    if userGuessResultIsContradictory fakeUserGuess guesses
    then do
        putStrLn $ "\nThe guess result is contradictory because it was: " ++ show fakeUserGuess ++ "\n"
        resolveUserGuessInExpertMode targetWord guesses curGuessNumber difficulty userGuess
        
    else resolveUserGuessByGuessResult targetWord guesses curGuessNumber difficulty fakeUserGuess   

resolveUserGuessByGuessResult :: String -> [UserGuess] -> Int -> GameDifficulty -> UserGuess -> IO ()
resolveUserGuessByGuessResult targetWord guesses curGuessNumber difficulty userGuessResult = do
    putStrLn $ getGuessResultAsString userGuessResult

    userInputGuess targetWord (userGuessResult:guesses) (curGuessNumber + 1) difficulty

resolveUserGuess :: String -> [UserGuess] -> Int -> GameDifficulty -> String -> IO ()
resolveUserGuess targetWord guesses curGuessNumber difficulty userGuess = do
    let guessResult = evaluateGuessResult targetWord userGuess

    resolveUserGuessByGuessResult targetWord guesses curGuessNumber difficulty guessResult

getRandomWord :: IO String
getRandomWord = do
    contents <- readFile wordsFileName

    let dictionary = lines contents
        randomNumberGen :: (RandomGen g) => Rand g Int
        randomNumberGen = getRandomR(0, length dictionary - 1)
        
    randomIndex <- evalRandIO randomNumberGen

    let randomWord = dictionary !! randomIndex

    --DEBUG
    putStrLn $ "\nThe secret word is " ++ randomWord ++ " but do not snitch ok!\n"

    return randomWord

userGuessResultIsContradictory :: UserGuess -> [UserGuess] -> Bool
userGuessResultIsContradictory (word, lettersInfo) oldGuesses = 
    let oldGuessesInfos = map (\(word, infos) -> infos) oldGuesses

        guessLetterIsContradictory :: LetterInfo -> [LetterInfo] -> Bool
        guessLetterIsContradictory (letter, status, index) wordInfo = any (\(l, s, i) -> letter == l && status /= s) wordInfo  

    in  any (\lf -> any (\oldGuessInfo -> guessLetterIsContradictory lf oldGuessInfo) oldGuessesInfos) lettersInfo


wordExists :: String -> [String] -> Bool
wordExists word dictionary =
    any (\x -> x == word) dictionary

getLetterAtIndex :: [LetterInfo] -> Int -> Maybe Char
getLetterAtIndex [] _ = Nothing
getLetterAtIndex letterInfos index = 
    let  lettersAtIndex = map (\(letter, status, curIndex) -> letter) 
            (filter (\(letter, status, curIndex) -> curIndex == index) letterInfos)

    in  if null lettersAtIndex then Nothing else Just (head lettersAtIndex)

wordDoesNotContainKnownGreenLetters :: String -> [UserGuess] -> Maybe String
wordDoesNotContainKnownGreenLetters guess oldGuesses = 
    let letterInfos = map (\(_, info) -> info) oldGuesses
        allGreenLetters = filter (\(letter, status, index) -> status == GREEN) (concat letterInfos)

        getGreenLetterAtIndex :: Int -> Maybe Char
        getGreenLetterAtIndex index = getLetterAtIndex allGreenLetters index

    in  if any (\i -> let greenLetter = getGreenLetterAtIndex i
                      in  isJust greenLetter && (guess !! i) /= fromJust greenLetter) [0..(length guess - 1)]
            then Just "\nThe word that you chose is missing known green letters."
        else Nothing


wordDoesNotContainNeededYellowLetters :: String -> [UserGuess] -> Maybe String
wordDoesNotContainNeededYellowLetters guess oldGuesses= 
    let allLetterInfos = concatMap (\(_, info) -> info) oldGuesses
        yellowLetters = filter (\(letter, status, _) -> status == YELLOW) allLetterInfos
    in  if not $ all (\(letter, _, _) -> letter `elem` guess) yellowLetters
            then Just "\nThe word that you chose is missing known yellow letters."
        else Nothing

wordContainsUselessGreyLetters :: String -> [UserGuess] -> Maybe String
wordContainsUselessGreyLetters guess oldGuesses = 
    let letterInfos = map (\(_, info) -> info) oldGuesses
    in  if any (\newLetter -> 
                    any (\letterInfo -> any (\(letter, status, _) -> newLetter == letter && status == GREY) letterInfo) letterInfos) guess 
            then Just "\nThe word that you chose has known grey letters."
        else Nothing

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

getGuessResultAsString :: UserGuess -> String
getGuessResultAsString (_, letters) = 
    let lettersToString = foldr 
            (\(l, s, _) acc -> l:(concat $ replicate (length (show s) - 1) " ") ++ " | " ++ acc) "" letters
        resultsToString = foldr (\(_, s, _) acc -> (show s) ++ " | " ++ acc) "" letters 
    in  lettersToString ++ "\n" ++ resultsToString 

-- hFlush stdout 
