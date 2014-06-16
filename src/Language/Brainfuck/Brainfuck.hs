
module Language.Brainfuck.Brainfuck where

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions

instr :: Parser (Maybe Instr)
instr = oneOf "<>+-.," >>= \inst -> return . Just $ case inst of
    '<' -> MoveLeft
    '>' -> MoveRight
    '+' -> Incr
    '-' -> Decr
    '.' -> Print
    ',' -> Read

loop :: Parser (Maybe Instr)
loop = between (char '[') (char ']') (program >>= return . Just . Loop)

comment :: Parser (Maybe Instr)
comment = noneOf "]" >> return Nothing

program :: Parser Program
program = liftM catMaybes $ many $ instr <|> loop <|> comment
