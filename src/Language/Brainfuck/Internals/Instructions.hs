
module Language.Brainfuck.Internals.Instructions where

data Instr =
    Incr
    | Decr
    | MoveRight
    | MoveLeft
    | Loop [Instr]
    | Read
    | Print
    | Nope
    deriving Show

type Program = [Instr]
