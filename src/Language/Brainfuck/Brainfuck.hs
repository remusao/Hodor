
module Language.Brainfuck.Brainfuck where

import Prelude ()
import Language.Brainfuck.Internals.GenericParser

program :: GenericParser
program = genparser Symbols {
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
