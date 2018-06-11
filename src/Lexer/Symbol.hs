module Lexer.Symbol where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Lexer.Parser
import qualified Text.Megaparsec.Char.Lexer as L

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parentheses :: Parser any -> Parser any
parentheses = between (symbol "(") (symbol ")")

semicolon :: Parser String
semicolon = symbol ";"