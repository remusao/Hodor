
module Language.Brainfuck.Internals.Instructions where

-- | ADT for Brainfuck's instructions
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

-- | A program is just a list of instructions
type Program = [Instr]
