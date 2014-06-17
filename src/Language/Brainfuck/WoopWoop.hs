
module Language.Brainfuck.WoopWoop where

import Prelude hiding (read, print)

import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions
import Language.Brainfuck.GenericParser

program :: Parser [Instr]
program = genparser $ Symbols {
    incr     = "Woop. Woop.",
    decr     = "Woop! Woop!",
    right    = "Woop. Woop?",
    left     = "Woop? Woop.",
    read     = "Woop. Woop!",
    print    = "Woop! Woop.",
    openl    = "Woop! Woop?",
    closel   = "Woop? Woop!",
    reserved = "Woop.!?"
}
