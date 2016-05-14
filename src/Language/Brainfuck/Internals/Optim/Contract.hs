module Language.Brainfuck.Internals.Optim.Contract where

import Language.Brainfuck.Internals.Instructions

optim :: Program -> Program
optim [] = []
optim (Incr i1:Incr i2:t) = optim $ Incr (i1 + i2):t
optim (Decr i1:Decr i2:t) = optim $ Decr (i1 + i2):t
optim (MoveRight n1:MoveRight n2:t) = optim $ MoveRight (n1 + n2):t
optim (MoveLeft n1:MoveLeft n2:t) = optim $ MoveLeft (n1 + n2):t
optim (Loop body:t) = Loop (optim body):optim t
optim (instr:t) = instr:optim t
