{-# LANGUAGE OverloadedStrings #-}

module Language.Brainfuck.Internals.CCodeGen where

import Data.Int
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Format
import Control.Monad.State
import Language.Brainfuck.Internals.Instructions


data CProgram = CProgram
    { statements :: TB.Builder
    , indent :: Int64 }

type Compiler = State CProgram ()


addLine :: T.Text -> CProgram -> CProgram
addLine l p = p { statements = statements p
                `mappend` (TB.fromLazyText (T.replicate (indent p) "    ")
                `mappend` (TB.fromLazyText l
                `mappend` TB.singleton '\n')) }

incrIndent :: CProgram -> CProgram
incrIndent p = p { indent=indent p + 1 }

decrIndent :: CProgram -> CProgram
decrIndent p = p { indent=indent p - 1 }

emptyProgram :: CProgram
emptyProgram = CProgram
    { statements=TB.fromLazyText ""
    , indent=1 }


gen :: Instr -> Compiler
gen (Incr i) = modify' . addLine . format "mem[ptr] += {};" $ Only i
gen (Decr i) = modify' . addLine . format "mem[ptr] -= {};" $ Only i
gen (Set i) = modify' . addLine . format "mem[ptr] = {};" $ Only i
gen (Mul x 1) = modify' . addLine . format "mem[ptr + {}] += mem[ptr];" $ Only x
gen (Mul x y) = modify' . addLine . format "mem[ptr + {}] += mem[ptr] * {};" $ (x, y)
gen (Copy n) = modify' . addLine . format "mem[ptr + {}] = mem[ptr]" $ Only n
gen (MoveRight n) = modify' . addLine . format "ptr += {};" $ Only n
gen (MoveLeft n) = modify' . addLine . format "ptr -= {};" $ Only n
gen Read = modify' (addLine "mem[ptr] = getchar();")
gen Print = modify' (addLine "putchar(mem[ptr]);")
gen (Loop body) = do
    modify' (addLine "while (mem[ptr]) {")
    modify' incrIndent
    mapM_ gen body
    modify' decrIndent
    modify' (addLine "}")
gen _ = return ()


genCode :: Program -> T.Text
genCode prog = TB.toLazyText . statements . execState (mapM_ gen prog) $ emptyProgram


wrapCode :: T.Text -> T.Text
wrapCode program = T.unlines
    [ "#include <stdio.h>"
    , ""
    , "int main() {"
    , "    char mem[30000] = {0};"
    , "    int ptr = 0;"
    , program
    , "}"
    ]


compile :: Program -> FilePath -> IO ()
compile [] _ = putStrLn "Empty program would result in empty file. No file created."
compile prog path = TIO.writeFile path . wrapCode . genCode $ prog
