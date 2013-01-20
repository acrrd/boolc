module Main(main) where

import Parser

import System.Environment
import System.Exit (exitSuccess)
import System.IO
import Control.Monad

printUsage :: IO ()
printUsage = do pname <- getProgName 
                putStrLn ("Usage: " ++ pname ++ " <file>")

parseFile :: String -> String -> String
parseFile fn fc = case (runBoolParser fn) fc of
                    Left e -> show e
                    Right v -> show v 

main :: IO ()
main = do args <- getArgs
          let nargs = length args
          when (nargs /= 1) (printUsage >> exitSuccess)
          let filename = args !! 0
          ret <- liftM (parseFile filename) $ readFile filename
          putStrLn ret
          return ()