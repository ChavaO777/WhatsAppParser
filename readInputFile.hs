import System.IO
import System.Environment

import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

import Data.Char ( isLetter )

removeNonLetters :: [Char] -> [Char]
removeNonLetters = filter isLetter 

-- computeWordCount :: String -> [(String, Int)]
-- computeWordCount = map (head &&& length) . group . sort . words . map toLower

-- displayWordCounts :: [IO String] -> [(String, Int)]
-- displayWordCounts [str] = computeWordCount str
-- displayWordCounts [str : strs] = computeWordCount [str]

main = do
    putStr ("Insert the text file name: ")
    fileName <- getLine
    fileContent <- readFile fileName
    putStrLn fileContent
    let fileLines = lines fileContent
    -- displayWordCounts fileLines
    let lineCount = length fileLines
    putStrLn ("Total messages: " ++ show lineCount)