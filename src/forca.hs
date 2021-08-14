import Data.List
import System.IO
import Data.Char
import System.Random (randomIO)
import Control.Applicative
import Display

main :: IO ()
main =
  do
    word <- menu
    game word maxError

game :: String -> Int -> IO ()
game word attemptTimes = 
  do
    putStrLn $ unlines $ hangDoll (maxError - attemptTimes)
    putStrLn $ showWord $ word

    finishGameOrContinue word attemptTimes

tryLetter :: String -> Char -> Int -> IO()
tryLetter word letter attemptTimes 
  | letter `elem` word = game [if letter == a then toUpper letter else a | a <- word] attemptTimes
  | otherwise = game word (attemptTimes - 1)

tryAgain :: String -> Int -> IO ()
tryAgain word attemptTimes =
  do
    putStrLn $ "Voce tem " ++ show attemptTimes ++ " tentativas restantes."
    putStrLn $ "Digite uma letra: "
    attemptLetter <- getLine
    tryLetter word (head attemptLetter) attemptTimes

finishGameOrContinue :: String -> Int -> IO ()
finishGameOrContinue word attemptTimes
  | attemptTimes == 0 = putStrLn "Você perdeu, tente novamente!"
  | not ('_' `elem` (showWord $ word)) = putStrLn "Parabéns, você acertou a palavra!"
  | otherwise = tryAgain word attemptTimes

maxError :: Int
maxError = length (forceDoll) - 1

drawWord :: String -> IO[Char]
drawWord wordsDictionary = do
  dictionary <- readFile wordsDictionary
  let words = lines dictionary
  let wordsLength = length words
  randomNumber <- randomIO
  let randomWord = words !! (randomNumber `mod` wordsLength)
  return $ randomWord

menu :: IO [Char]
menu =
  do
    putStrLn "\nSelecione um tema:"
    putStrLn "1. Animais"
    putStrLn "2. Frutas"
    putStrLn "3. Profissoes"
    putStrLn "4. Todos\n"

    putStr "Opcao: "
    op <- getLine

    word <- case op of  "1" -> drawWord "./database/animais.txt";
                        "2" -> drawWord "./database/frutas.txt";
                        "3" -> drawWord "./database/profissoes.txt";
                        "4" -> drawWord "./database/todos.txt";
    return word
