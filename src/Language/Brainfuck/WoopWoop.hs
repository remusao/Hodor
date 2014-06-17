
module Language.Brainfuck.Woop where

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

incr = parseOp "Woop." "Woop." Incr
decr = parseOp "Woop!" "Woop!" Decr
right = parseOp "Woop." "Woop?" MoveRight
left = parseOp "Woop?" "Woop." MoveLeft
read = parseOp "Woop." "Woop!" Read
print = parseOp "Woop!" "Woop." Print
eop = parseOp "Woop?" "Woop?" Nope

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
        open = (try $ parseOp "Woop!" "Woop?" Nope)
        close = (try $ parseOp "Woop?" "Woop!" Nope)

comment :: Parser (Maybe Instr)
comment = noneOf "Woop.!?" >> return Nothing

program :: Parser [Instr]
program = liftM catMaybes $ many $ instr <|> loop <|> comment
