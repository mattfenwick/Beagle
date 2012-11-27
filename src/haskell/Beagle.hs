import Parser
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import TParse (getParser, end, (<*))


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents



main = 
    putStrLn "please enter file name:"   >>
    getLine                              >>= \name -> 
    myReadFile name                      >>=
    putStrLn . show . full
  where
    p = scanner <* end


