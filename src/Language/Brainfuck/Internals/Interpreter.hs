
module Language.Brainfuck.Internals.Interpreter where

import Data.Char (ord, chr)
import Control.Monad.State
import Language.Brainfuck.Internals.Instructions

-- | Memory is just a list zipper
data Memory = Memory [Int] Int [Int]

empty :: Memory
empty = Memory zeros 0 zeros
    where zeros = repeat 0

-- | Increment current value in memory
incr :: Memory -> Memory
incr (Memory l v r) = Memory l (v + 1) r

-- | Decrement current value in memory
decr :: Memory -> Memory
decr (Memory l v r) = Memory l (v - 1) r

-- | Shift zipper to left of one position
--   If we are already at the left, don't move
--   (Note that if memory were initialized with
--    `empty` this case never happens)
left :: Memory -> Memory
left m@(Memory [] _ _) = m
left (Memory (l:ls) v r) = Memory ls l (v:r)

-- | Shift zipper to right of one position
--   If we are already at the right, don't move
--   (Note that if memory were initialized with
--    `empty` this case never happens)
right :: Memory -> Memory
right r@(Memory _ _ []) = r
right (Memory l v (r:rs)) = Memory (v:l) r rs

-- | Returns current value
getVal :: Memory -> Int
getVal (Memory _ v _) = v

-- | Set current value
setVal :: Int -> Memory -> Memory
setVal new_v (Memory l _ r) = Memory l new_v r

-- | Interpreter internal state
type Interpreter = StateT Memory IO

-- | Execute one instruction
run :: Instr -> Interpreter ()
run Incr = modify incr
run Decr = modify decr
run MoveRight = modify right
run MoveLeft = modify left
run Read = liftIO getChar >>= modify . setVal . ord
run Print = gets getVal >>= liftIO . putStr . return . chr
run l@(Loop body) = do
    val <- gets getVal
    unless (val == 0) $ mapM_ run body >> run l
run _ = return ()

-- | Interpret a complete program
interpret :: Program -> IO ()
interpret [] = return ()
interpret prog = evalStateT (mapM_ run prog) empty
