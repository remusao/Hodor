module Main where

import System.Environment
import Text.Parsec.String (parseFromFile)

import Language.Brainfuck.Internals.Interpreter
import qualified Language.Brainfuck.Brainfuck as BF
import qualified Language.Brainfuck.OokOok as OO
import qualified Language.Brainfuck.Hodor as H
import qualified Language.Brainfuck.WoopWoop as W
import qualified Language.Brainfuck.Buffalo as B


main :: IO ()
main = do
    [lang, file] <- getArgs
    result <- parseFromFile (parser lang) file
    case result of
        Left err -> putStr "parse error at " >> print err
        Right x -> interpret x
    where
        parser lang = case lang of
            "Brainfuck" -> BF.program
            "OokOok" -> OO.program
            "Hodor" -> H.program
            "WoopWoop" -> W.program
            "Buffalo" -> B.program
            _ -> error "Not a known language"
