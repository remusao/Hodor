
module Language.Brainfuck.Internals.Instructions where

-- | ADT for Brainfuck's instructions
data Instr =
      Incr Int
    | Decr Int
    | MoveRight Int
    | MoveLeft Int
    | Loop [Instr]
    | Read
    | Print
    | Nope
    deriving Show

-- | A program is just a list of instructions
type Program = [Instr]
