
module Language.Brainfuck.OokOok where

import Prelude ()
import Language.Brainfuck.Internals.GenericParser

program :: GenericParser
program = genparser Symbols {
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
