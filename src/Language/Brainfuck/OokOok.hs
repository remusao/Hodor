
module Language.Brainfuck.OokOok where

import Prelude hiding (read, print)

import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions
import Language.Brainfuck.GenericParser

program :: Parser [Instr]
program = genparser $ Symbols {
    incr     = "Ook. Ook.",
    decr     = "Ook! Ook!",
    right    = "Ook. Ook?",
    left     = "Ook? Ook.",
    read     = "Ook. Ook!",
    print    = "Ook! Ook.",
    openl    = "Ook! Ook?",
    closel   = "Ook? Ook!",
    reserved = "Ook.!?"
}
