
module Language.Brainfuck.Internals.CCodeGen where

import Control.Monad.State
import Language.Brainfuck.Internals.Instructions


data CProgram = CProgram { statements :: [String] }
type Compiler = State CProgram ()


addLine :: String -> CProgram -> CProgram
addLine l p = CProgram { statements = statements p ++ [l] }


emptyProgram :: CProgram
emptyProgram = CProgram { statements=[] }


gen :: Instr -> Compiler
gen Incr = modify' (addLine "++(*ptr);")
gen Decr = modify' (addLine "--(*ptr);")
gen MoveRight = modify' (addLine "++ptr;")
gen MoveLeft = modify' (addLine "--ptr;")
gen Read = modify' (addLine "*ptr = getchar();")
gen Print = modify' (addLine "putchar(*ptr);")
gen (Loop body) = do
    modify' (addLine "while (*ptr) {")
    mapM_ gen body
    modify' (addLine "}")
gen _ = return ()


genCode :: Program -> String
genCode prog = unwords . statements . execState (mapM_ gen prog) $ emptyProgram


wrapCode :: String -> String
wrapCode program = unlines $ [
    "#include <stdio.h>",
    "",
    "int main() {",
    "    char array[30000] = {0};",
    "    char *ptr=array;",
    program,
    "}"
    ]


compile :: Program -> FilePath -> IO ()
compile [] _ = putStrLn "Empty program would result in empty file. No file created."
compile prog path = writeFile path . wrapCode . genCode $ prog
