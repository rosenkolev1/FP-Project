module GameMode where

import System.IO
import Control.Monad.Random
import Data.Semigroup (diff)
import Data.Char (chr)
import Text.Read (Lexeme(String))
import Data.Maybe (fromJust, isJust)
import Common

startGameMode :: IO ()
startGameMode = do
    putStrLn $ "\nWhich difficulty would you like to play in: (" ++ easyDifficulty
        ++ "/" ++ normalDifficulty ++ "/" ++ expertDifficulty ++ ")"
    gameDifficulty <- getLine

    if gameDifficulty == easyDifficulty
        then do
            putStrLn "\nThe game is on easy difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 EasyGame False

    else if gameDifficulty == normalDifficulty
        then do 
            putStrLn "\nThe game is on normal difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 NormalGame False

    else if gameDifficulty == expertDifficulty 
        then do
            putStrLn "\nThe game is on EXPERT difficulty!"

            --Get a random word from the text file
            targetWord <- getRandomWord

            userInputGuess targetWord [] 1 ExpertGame False

    else do
        putStrLn invalidCommand
        startGameMode

userInputGuess :: String -> [GuessResult] -> Int -> GameDifficulty -> Bool -> IO ()
userInputGuess targetWord guesses curGuessNumber difficulty hasLiedToUser = do

    if curGuessNumber > 6
        then do
            putStrLn "\nYou have lost the game! Sucks to be you loser!"
            return ()
    else do
        putStrAndFlush $ "\nRemaining Guesses " ++ show (7 - curGuessNumber) ++ "\nGuess " ++ show curGuessNumber ++ ": "

        userGuess <- getLine
        contents <- readFile wordsFileNameGameMode

        if userGuess == targetWord
            then do
                putStrLn "\nYou have guessed the word correctly! Congrats!"
                return ()
        else if length userGuess /= wordsLength
            then do
                putStrLn invalidCommand
                userInputGuess targetWord guesses curGuessNumber difficulty hasLiedToUser
        else do
            let dictionary = lines contents
                guessExistsInDictionary = wordExists userGuess dictionary

            --Check if the word doesn't exist and the game is in easy diff
            if difficulty == EasyGame && not guessExistsInDictionary
                then do
                    putStrLn "\nThe word doesn't exist in the dictionary!"
                    userInputGuess targetWord guesses curGuessNumber difficulty hasLiedToUser

            --Check if the word doesn't exist and the game is not in easy diff
            else if difficulty /= EasyGame && not guessExistsInDictionary
                then do
                    putStrLn "\nThe word doesn't exist in the dictionary! Taking away a guess hehe!"
                    userInputGuess targetWord guesses (curGuessNumber + 1) difficulty hasLiedToUser
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
                            then userInputGuess targetWord guesses curGuessNumber difficulty hasLiedToUser

                        else resolveGuessResult targetWord guesses curGuessNumber difficulty userGuess hasLiedToUser

                else if difficulty == ExpertGame 
                    then do 
                        resolveGuessResultInExpertMode targetWord guesses curGuessNumber difficulty userGuess hasLiedToUser       

                else resolveGuessResult targetWord guesses curGuessNumber difficulty userGuess hasLiedToUser

willLieToUser :: Int -> Bool -> IO Bool
willLieToUser curGuessNumber hasLied = do
    if hasLied 
        then do 
            putStrLn "\nYou have lied already!\n" 
            return False
    else do
        randomNumber <- evalRandIO $ getRandomR (1, 6)
        return $ randomNumber <= curGuessNumber

resolveGuessResultInExpertMode :: String -> [GuessResult] -> Int -> GameDifficulty -> String -> Bool -> IO ()
resolveGuessResultInExpertMode targetWord guesses curGuessNumber difficulty userGuess hasLied = do

    willLie <- willLieToUser curGuessNumber hasLied

    if willLie
        then do
            --Get all the targetWords which would return the same old guesses
            content <- readFile wordsFileNameGameMode

            let allPossibleTargets = 
                    foldr (\newTargetWord acc -> 
                        let evalResults = foldr (\(guessWord, _) acc2 -> (evaluateGuessResult newTargetWord guessWord):acc2) [] guesses
                        in  if evalResults == guesses then newTargetWord:acc else acc) 
                            [] (lines content)

            if length allPossibleTargets <= 1
                then do
                    putStrLn $ "\nThere are no words that could match the current guesses other than the real word" ++ "\n"
                    resolveGuessResult targetWord guesses curGuessNumber difficulty userGuess hasLied
            else do
                randomNumber <- evalRandIO $ getRandomR (0, length allPossibleTargets - 1)

                let fakeTargetWord = allPossibleTargets !! randomNumber    
                    fakeEvaluation = evaluateGuessResult fakeTargetWord userGuess

                putStrLn $ "\nThe fake target word is: " ++ fakeTargetWord ++ "\n"

                resolveGuessResultByGuessResult targetWord guesses curGuessNumber difficulty fakeEvaluation True      

    else resolveGuessResult targetWord guesses curGuessNumber difficulty userGuess hasLied
    

resolveGuessResultByGuessResult :: String -> [GuessResult] -> Int -> GameDifficulty -> GuessResult -> Bool -> IO ()
resolveGuessResultByGuessResult targetWord guesses curGuessNumber difficulty userGuessResult hasLiedToUser = do
    putStrLn $ getGuessResultAsString userGuessResult

    userInputGuess targetWord (userGuessResult:guesses) (curGuessNumber + 1) difficulty hasLiedToUser

resolveGuessResult :: String -> [GuessResult] -> Int -> GameDifficulty -> String -> Bool -> IO ()
resolveGuessResult targetWord guesses curGuessNumber difficulty userGuess hasLied = do
    let guessResult = evaluateGuessResult targetWord userGuess

    resolveGuessResultByGuessResult targetWord guesses curGuessNumber difficulty guessResult hasLied

getRandomWord :: IO String
getRandomWord = do
    contents <- readFile wordsFileNameGameMode

    let dictionary = lines contents
        randomNumberGen :: (RandomGen g) => Rand g Int
        randomNumberGen = getRandomR(0, length dictionary - 1)
        
    randomIndex <- evalRandIO randomNumberGen

    let randomWord = dictionary !! randomIndex

    --DEBUG
    putStrLn $ "\nThe secret word is " ++ randomWord ++ " but do not snitch ok!\n"

    return randomWord

wordExists :: String -> [String] -> Bool
wordExists word dictionary =
    any (\x -> x == word) dictionary

wordDoesNotContainKnownGreenLetters :: String -> [GuessResult] -> Maybe String
wordDoesNotContainKnownGreenLetters guess oldGuesses = 
    let letterInfos = map (\(_, info) -> info) oldGuesses

        getLetterAtIndex :: [LetterInfo] -> Int -> Maybe Char
        getLetterAtIndex [] _ = Nothing
        getLetterAtIndex letterInfos index = 
            let  lettersAtIndex = map (\(letter, status, curIndex) -> letter) 
                    (filter (\(letter, status, curIndex) -> curIndex == index) letterInfos)

            in  if null lettersAtIndex then Nothing else Just (head lettersAtIndex)

        allGreenLetters = filter (\(letter, status, index) -> status == GREEN) (concat letterInfos)

        getGreenLetterAtIndex :: Int -> Maybe Char
        getGreenLetterAtIndex index = getLetterAtIndex allGreenLetters index

    in  if any (\i -> let greenLetter = getGreenLetterAtIndex i
                      in  isJust greenLetter && (guess !! i) /= fromJust greenLetter) [0..(length guess - 1)]
            then Just "\nThe word that you chose is missing known green letters."
        else Nothing


wordDoesNotContainNeededYellowLetters :: String -> [GuessResult] -> Maybe String
wordDoesNotContainNeededYellowLetters guess oldGuesses= 
    let allLetterInfos = concatMap (\(_, info) -> info) oldGuesses
        yellowLetters = filter (\(letter, status, _) -> status == YELLOW) allLetterInfos
    in  if not $ all (\(letter, _, _) -> letter `elem` guess) yellowLetters
            then Just "\nThe word that you chose is missing known yellow letters."
        else Nothing

wordContainsUselessGreyLetters :: String -> [GuessResult] -> Maybe String
wordContainsUselessGreyLetters guess oldGuesses = 
    let letterInfos = map (\(_, info) -> info) oldGuesses
    in  if any (\newLetter -> 
                    any (\letterInfo -> any (\(letter, status, _) -> newLetter == letter && status == GREY) letterInfo) letterInfos) guess 
            then Just "\nThe word that you chose has known grey letters."
        else Nothing

