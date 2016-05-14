
module Language.Brainfuck.Internals.Optim.ClearLoop where

import Language.Brainfuck.Internals.Instructions

optim :: Program -> Program
optim [] = []
optim (Loop [Incr _]:t) = Set 0 : optim t
optim (Loop [Decr _]:t) = Set 0 : optim t
optim (Loop body:t) = Loop (optim body) : optim t
optim (instr:t) = instr:optim t
