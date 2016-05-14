
module Language.Brainfuck.Internals.Instructions where

-- | ADT for Brainfuck's instructions
type Offset = Int
data Instr =
      Incr Int
    | Decr Int
    | MoveRight Offset
    | MoveLeft Offset
    | Loop [Instr]
    | Read
    | Print
    | Set Int
    | Copy Offset
    | Mul Int Int
    | Nope
    deriving Show

-- | A program is just a list of instructions
type Program = [Instr]
