
module Language.Brainfuck.Buffalo where

import Prelude ()
import Language.Brainfuck.Internals.GenericParser

program :: GenericParser
program = genparser Symbols {
    incr     = "buffalo buffalo",
    decr     = "buffalo Buffalo",
    right    = "Buffalo buffalo",
    left     = "Buffalo Buffalo",
    read     = "Buffalo Buuffalo",
    print    = "buffalo Buuffalo",
    openl    = "Buuffalo Buuffalo",
    closel   = "Buuffalo buuffalo",
    reserved = "bBuffalo"
}
