
module Language.Brainfuck.Hodor where

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
    
incr = parseOp "Hodor." "Hodor." Incr
decr = parseOp "Hodor!" "Hodor!" Decr
right = parseOp "Hodor." "Hodor?" MoveRight
left = parseOp "Hodor?" "Hodor." MoveLeft
read = parseOp "Hodor." "Hodor!" Read
print = parseOp "Hodor!" "Hodor." Print
eop = parseOp "Hodor?" "Hodor?" Nope

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
        open = (try $ parseOp "Hodor!" "Hodor?" Nope)
        close = (try $ parseOp "Hodor?" "Hodor!" Nope)

comment :: Parser (Maybe Instr)
comment = noneOf "Hodor.!?" >> return Nothing

program :: Parser [Instr]
program = liftM catMaybes $ many $ instr <|> loop <|> comment
