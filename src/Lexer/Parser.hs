module Lexer.Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

spaceConsumer :: Parser () 
spaceConsumer = L.space space1 lineComment blockComment 
    where
        lineComment = L.skipLineComment "//"
        blockComment = empty

lexeme :: Parser any -> Parser any
lexeme = L.lexeme spaceConsumer

