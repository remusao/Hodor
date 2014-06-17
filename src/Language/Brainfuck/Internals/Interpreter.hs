
module Language.Brainfuck.Internals.Interpreter where

import Data.Char (ord, chr)
import Control.Monad.State
import Language.Brainfuck.Internals.Instructions

-- Memory
-- It's a list zipper
type Word = Int
data Memory = Memory [Word] Word [Word]

empty :: Memory
empty = Memory zeros 0 zeros
    where zeros = repeat 0

incr :: Memory -> Memory
incr (Memory l v r) = Memory l (v + 1) r

decr :: Memory -> Memory
decr (Memory l v r) = Memory l (v - 1) r

left :: Memory -> Memory
left (Memory (l:ls) v r) = Memory ls l (v:r)

right :: Memory -> Memory
right (Memory l v (r:rs)) = Memory (v:l) r rs

getVal :: Memory -> Word
getVal (Memory _ v _) = v

setVal :: Word -> Memory -> Memory
setVal new_v (Memory l _ r) = Memory l new_v r

-- Interpreter
type Interpreter = StateT Memory IO

runInstr :: Instr -> Interpreter ()
runInstr Incr = modify incr
runInstr Decr = modify decr
runInstr MoveRight = modify right
runInstr MoveLeft = modify left
runInstr Read = liftIO getChar >>= modify . setVal . ord
runInstr Print = gets getVal >>= liftIO . putStr . return . chr
runInstr l@(Loop body) = do
    val <- gets getVal
    if val == 0
    then return ()
    else mapM_ runInstr body >> runInstr l
runInstr _ = return ()


interpret :: Program -> IO ()
interpret [] = return ()
interpret prog = evalStateT (mapM_ runInstr prog) empty
