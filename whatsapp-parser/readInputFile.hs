import System.IO
import System.Environment

import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . words . map toLower

main = do
    [fileName] <- getArgs
    fileContent <- readFile fileName
    putStrLn fileContent
    let lineCount = length (lines fileContent)
    putStrLn ("Total messages: " ++ show lineCount)
    let a = wordCount ((lines fileContent) !! 0)
    putStrLn (show a)