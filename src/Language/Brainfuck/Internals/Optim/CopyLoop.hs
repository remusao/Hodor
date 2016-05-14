{-# LANGUAGE LambdaCase #-}

module Language.Brainfuck.Internals.Optim.CopyLoop where

import Language.Brainfuck.Internals.Instructions
import qualified Data.Map as M
import Control.Monad.State


-- | Loop is optimizable only if it contains only basic operations:
-- * Incr
-- * Decr
-- * MoveLeft
-- * MoveRight
optimizableLoop :: [Instr] -> Bool
optimizableLoop [] = True
optimizableLoop (Incr _:t) = True && optimizableLoop t
optimizableLoop (Decr _:t) = True && optimizableLoop t
optimizableLoop (MoveLeft _:t) = True && optimizableLoop t
optimizableLoop (MoveRight _:t) = True && optimizableLoop t
optimizableLoop _ = False


-- | Interpret loop and keep track of `ptr` and `memory` to detect copy
data Interpreter = Interpreter
    { memory:: M.Map Int Int
    , ptr::Int}

defaultInterpreter :: Interpreter
defaultInterpreter = Interpreter {memory = M.empty, ptr = 0}

interpret :: Instr -> State Interpreter ()
interpret (Incr i) = modify' (\s -> s { memory=M.alter (\case { Nothing -> Just i; Just v -> Just (v + i) }) (ptr s) (memory s) })
interpret (Decr i) = modify' (\s -> s { memory=M.alter (\case { Nothing -> Just (-1 * i); Just v -> Just (v - i) }) (ptr s) (memory s) })
interpret (MoveRight n) = modify' (\s -> s { ptr=n + ptr s })
interpret (MoveLeft n) = modify' (\s -> s { ptr=(-n) + ptr s})
interpret _ = return ()


optim :: Program -> Program
optim [] = []
optim (Loop body:t)
    | optimizableLoop body =
        let s = execState (mapM_ interpret body) defaultInterpreter
        in if ptr s == 0 && M.findWithDefault 0 0 (memory s) == -1
              then [Mul p val | (p, val) <- M.toAscList (memory s), p /= 0] ++ Set 0 :optim t
              else Loop body : optim t
    | otherwise = Loop (optim body) : optim t
optim (instr:t) = instr:optim t
