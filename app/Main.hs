{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Main where


import Options.Generic
import System.Environment
import Text.Parsec.String (parseFromFile)

import qualified Language.Brainfuck.Internals.Interpreter as Interpreter
import qualified Language.Brainfuck.Internals.CCodeGen as Compiler

import qualified Language.Brainfuck.Internals.Optim.Contract as Contract
import qualified Language.Brainfuck.Internals.Optim.Cancel as Cancel
import qualified Language.Brainfuck.Internals.Optim.ClearLoop as ClearLoop
import qualified Language.Brainfuck.Internals.Optim.CopyLoop as CopyLoop

import qualified Language.Brainfuck.Brainfuck as BF
import qualified Language.Brainfuck.OokOok as OO
import qualified Language.Brainfuck.Hodor as H
import qualified Language.Brainfuck.WoopWoop as W
import qualified Language.Brainfuck.Buffalo as B

-- Allows brainfuck dialects
data Dialect =
    Brainfuck
    | OokOok
    | Hodor
    | WoopWoop
    | Buffalo
    deriving (Generic, Show, Read)

-- Argument parser
data Arguments = Arguments
    { compile :: Bool     -- <?> "Compile programming using LLVM"
    , dialect :: Dialect  -- <?> "One of: Brainfuck, OokOok, Hodor, WoopWoop, Buffalo."
    , file    :: FilePath -- <?> "Source of program."
    } deriving (Generic, Show)

instance ParseField Dialect
instance ParseFields Dialect
instance ParseRecord Dialect
instance ParseRecord Arguments


main :: IO ()
main = do
    args <- getRecord "Hodor: brainfuck interpreter and compiler"
    result <- parseFromFile (parser . dialect $ args) (file args)
    case result of
        Left err -> putStr "parse error at " >> print err
        Right x -> if compile args
                      then Compiler.compile (opti x) "out.c"
                      else Interpreter.interpret x
    where
        parser lang = case lang of
            Brainfuck -> BF.program
            OokOok -> OO.program
            Hodor -> H.program
            WoopWoop -> W.program
            Buffalo -> B.program
        opti = Contract.optim . Cancel.optim . ClearLoop.optim . CopyLoop.optim
