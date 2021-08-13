module Display where

import Data.List (transpose)
import Data.List (intersperse)
import Control.Applicative


forceDoll :: [[String]]
forceDoll = 
    transpose
    [["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
    ,["   ", "   ", " | ", " | ", " | " , "/| " , "/|\\" ]
    ,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]


hangDoll :: Int -> [String]
hangDoll index =
    "-----------" :
    "|    |" :
    map ("|   " ++) image
    where image = forceDoll !! index

showWord :: String -> String
showWord word = intersperse ' ' [if a `elem` ['a'..'z'] then '_' else a | a <- word]
