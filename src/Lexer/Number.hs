module Lexer.Number where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Lexer.Parser
import qualified Text.Megaparsec.Char.Lexer as L

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed spaceConsumer integer

float :: Parser Float
float = lexeme L.float

signedFloat :: Parser Float
signedFloat = L.signed spaceConsumer float