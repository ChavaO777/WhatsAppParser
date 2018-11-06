import System.IO
import System.Environment

main = do
    [fileName] <- getArgs
    fileContent <- readFile fileName
    putStrLn fileContent
    let lineCount = length (lines fileContent)
    putStrLn ("Total messages: " ++ show lineCount)