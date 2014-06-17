
module Language.Brainfuck.WoopWoop where

import Prelude ()
import Language.Brainfuck.Internals.GenericParser

program :: GenericParser
program = genparser Symbols {
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
