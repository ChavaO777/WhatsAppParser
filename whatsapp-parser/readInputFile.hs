import System.IO
import System.Environment

main = do
    [fileName] <- getArgs
    dat <- readFile fileName
    putStrLn dat

    handle <- openFile fileName ReadMode
    hClose handle