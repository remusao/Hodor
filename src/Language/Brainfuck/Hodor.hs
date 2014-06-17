
module Language.Brainfuck.Hodor where

import Prelude hiding (read, print)

import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions
import Language.Brainfuck.GenericParser

program :: Parser [Instr]
program = genparser $ Symbols {
    incr     = "Hodor. Hodor.",
    decr     = "Hodor! Hodor!",
    right    = "Hodor. Hodor?",
    left     = "Hodor? Hodor.",
    read     = "Hodor. Hodor!",
    print    = "Hodor! Hodor.",
    openl    = "Hodor! Hodor?",
    closel   = "Hodor? Hodor!",
    reserved = "Hodor.!?"
}
