import System.IO
import Data.Char

main =
  do
    let tempLetterSize = 5
    displayHangman
    putStr "  "
    displayLetters tempLetterSize
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