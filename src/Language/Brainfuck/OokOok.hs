
module Language.Brainfuck.OokOok where

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
    
incr = parseOp "Ook." "Ook." Incr
decr = parseOp "Ook!" "Ook!" Decr
right = parseOp "Ook." "Ook?" MoveRight
left = parseOp "Ook?" "Ook." MoveLeft
read = parseOp "Ook." "Ook!" Read
print = parseOp "Ook!" "Ook." Print
eop = parseOp "Ook?" "Ook?" Nope

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
comment = noneOf "Ook.!?" >> return Nothing

program :: Parser [Instr]
program = liftM catMaybes $ many $ instr <|> loop <|> comment
