
module Main where

import System.Console.GetOpt
import System.Environment
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Text.Parsec.String as TP

import Language.Brainfuck.Interpreter
import qualified Language.Brainfuck.Brainfuck as BF
import qualified Language.Brainfuck.OokOok as OO
import qualified Language.Brainfuck.Hodor as H
import qualified Language.Brainfuck.WoopWoop as W


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
            _ -> error "Not a known language"
