
module Language.Brainfuck.Brainfuck where

import Prelude hiding (read, print)

import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions
import Language.Brainfuck.GenericParser

program :: Parser [Instr]
program = genparser $ Symbols {
    incr     = "+",
    decr     = "-",
    right    = ">",
    left     = "<",
    read     = ",",
    print    = ".",
    openl    = "[",
    closel   = "]",
    reserved = "[]+-<>.,"
}
