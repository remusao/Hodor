
module Language.Brainfuck.Internals.Optim.Cancel where

import Language.Brainfuck.Internals.Instructions


combine :: (Int -> Instr) -> Int -> (Int -> Instr) -> Int -> Instr
combine f1 i1 f2 i2
    | i1 == i2  = Nope
    | i1 > i2   = f1 (i1 - i2)
    | otherwise = f2 (i2 - i1)


optim :: Program -> Program
optim [] = []
optim (Incr i1:Decr i2:t) = optim $ combine Incr i1 Decr i2 : t
optim (Decr i1:Incr i2:t) = optim $ combine Decr i1 Incr i2 : t
optim (MoveRight n1:MoveLeft n2:t) = optim $ combine MoveRight n1 MoveLeft n2 : t
optim (MoveLeft n1:MoveRight n2:t) = optim $ combine MoveLeft n1 MoveRight n2 : t
optim (Loop body:t) = Loop (optim body):optim t
optim (instr:t) = instr:optim t
