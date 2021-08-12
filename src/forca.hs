import Data.List
import System.IO
import Data.Char
import Data.List (transpose)
import System.Random (randomIO)
import Control.Applicative

main :: IO ()
main =
  do
    let tempLetterSize = 5
    displayHangman
    putStr "  "
    displayLetters tempLetterSize
    r <- menu
    putStr " Digite a letra: "
    readChar

readChar =
  do
    charInput <- getChar
    putStrLn ""

displayHangman =
  putStrLn " _ _ _ _ _\n|\n|\n|\n|\n|\n|"

displayLetters 0 = putStrLn ""
displayLetters lettersnumber =
  do
    putStr "__  "
    displayLetters (lettersnumber - 1)

drawWord :: String -> IO[Char]
drawWord wordsDictionary = do
  dictionary <- readFile wordsDictionary
  let words = lines dictionary
  let wordsLength = length words
  print wordsLength
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