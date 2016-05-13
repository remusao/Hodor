
module Language.Brainfuck.Buffalo where

import Prelude hiding (read, print)

import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions

parseOp :: String -> String -> Instr -> Parser (Maybe Instr)
parseOp s1 s2 instr = do
    _ <- string s1
    _ <- skipMany comment
    _ <- string s2
    return . Just $ instr
    
incr = parseOp "buffalo" "buffalo" Incr
decr = parseOp "buffalo" "Buffalo" Decr
right = parseOp "Buffalo" "buffalo" MoveRight
left = parseOp "Buffalo" "Buffalo" MoveLeft
read = parseOp "Buffalo" "Buuffalo" Read
print = parseOp "buffalo" "Buuffalo" Print
eop = parseOp "Buuffalo" "Buuffalo" Nope

instr :: Parser (Maybe Instr)
instr = (try incr <|>
         try decr <|>
         try right <|>
         try left <|>
         try read <|>
         try print)

loop :: Parser (Maybe Instr)
loop = between open close (program >>= return . Just . Loop)
    where
        open = (try $ parseOp "Ook!" "Ook?" Nope)
        close = (try $ parseOp "Ook?" "Ook!" Nope)

comment :: Parser (Maybe Instr)
comment = noneOf "bBuffalo" >> return Nothing

program :: Parser [Instr]
program = liftM catMaybes $ many $ instr <|> loop <|> comment

