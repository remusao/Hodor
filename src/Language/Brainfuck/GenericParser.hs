
module Language.Brainfuck.GenericParser where

import Prelude hiding (read, print)

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Language.Brainfuck.Instructions

-- Record type containing symbols of Brainfuck language
data Symbols = Symbols {
    incr :: String,
    decr :: String,
    right :: String,
    left :: String,
    read :: String,
    print :: String,
    openl :: String,
    closel :: String,
    reserved :: String
}

-- Used to generate a parser for a Brainfuck's dialect
genparser :: Symbols -> Parser [Instr]
genparser sym =
    let loop = between
            (string $ openl sym)            -- Open loop
            (string $ closel sym)           -- Close loop
            (Just . Loop <$> genparser sym) -- Body
        instr = choice [
            parseInstr (incr sym) Incr,
            parseInstr (decr sym) Decr,
            parseInstr (right sym) MoveRight,
            parseInstr (left sym) MoveLeft,
            parseInstr (read sym) Read,
            parseInstr (print sym) Print]
        comment = noneOf (reserved sym) >> return Nothing
    in fmap catMaybes $ many $ try instr <|> try loop <|> comment
    where
        parseInstr str instr = try $ do
            _ <- string str
            return $ Just instr
