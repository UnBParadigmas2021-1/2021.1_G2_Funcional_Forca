import System.IO
import Data.Char
import Control.Monad

main =
  do
    let tempLetterSize = 5
    displayHangman
    putStr "  "
    displayLetters tempLetterSize
    r <- menu
    print r
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

menu =
  do
    putStrLn "\nSelecione um tema:"
    putStrLn "1. Animais"
    putStrLn "2. Frutas"
    putStrLn "3. Profissoes"
    putStrLn "4. Todos\n"
    
    putStr "Opcao: "
    op <- getLine
    
    contents <- case op of  "1" -> readFile "src/database/animais.txt";
                            "2" -> readFile "src/database/frutas.txt";
                            "3" -> readFile "src/database/profissoes.txt";
                            "4" -> readFile "src/database/todos.txt";
    return contents