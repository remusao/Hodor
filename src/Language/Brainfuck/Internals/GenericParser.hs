
module Language.Brainfuck.Internals.GenericParser where

import Prelude hiding (print, read)

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Language.Brainfuck.Internals.Instructions

-- | Encapsulate a parser
type GenericParser = Parser [Instr]

-- | Record type containing symbols of Brainfuck language
data Symbols = Symbols {
    incr :: String,     -- Symbol for +
    decr :: String,     -- Symbol for -
    right :: String,    -- Symbol for >
    left :: String,     -- Symbol for <
    read :: String,     -- Symbol for ,
    print :: String,    -- Symbol for .
    openl :: String,    -- Symbol for [
    closel :: String,   -- Symbol for ]
    reserved :: String  -- None of this is a comment
}

-- | Used to generate a parser for a Brainfuck's dialect
genparser :: Symbols -> GenericParser
genparser sym = fmap catMaybes $ many $ try instr <|> try loop <|> comment
    where
        comment = noneOf (reserved sym) >> return Nothing
        instr = choice [
            parseInstr incr (Incr 1),
            parseInstr decr (Decr 1),
            parseInstr right (MoveRight 1),
            parseInstr left (MoveLeft 1),
            parseInstr read Read,
            parseInstr print Print]
        loop = between
            (string $ openl sym)            -- Open loop
            (string $ closel sym)           -- Close loop
            (Just . Loop <$> genparser sym) -- Body
        parseInstr fun inst = try $ do
            _ <- string $ fun sym
            return $ Just inst
