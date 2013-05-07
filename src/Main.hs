module Main(main) where

import Ast
import AstUtils
import Parser
import Typesystem
import ToLLVM

import Text.ParserCombinators.Parsec (ParseError)

import Control.Monad

import System.Environment
import System.Exit (exitSuccess)
import System.IO
import System.FilePath

printUsage :: IO ()
printUsage = do pname <- getProgName 
                putStrLn ("Usage: " ++ pname ++ " <file>")

parseFile :: String -> IO (Either ParseError ProgramSP)
parseFile fn = liftM (runBoolParser fn) $ readFile fn

main :: IO ()
main = do args <- getArgs
          let nargs = length args
          when (nargs /= 1) (printUsage >> exitSuccess)
          let filepath = args !! 0
          let fileoutput = replaceExtension (snd $ splitFileName filepath) ".bc"
          parseresult <- parseFile filepath
          case parseresult of
            Left perr -> putStrLn $ show perr
            Right ast -> do let ast' = desugar ast
                            case typecheck ast' of
                              Left terr -> putStrLn $ show terr
                              Right (astt,cte) -> genLLVMCode astt cte fileoutput

