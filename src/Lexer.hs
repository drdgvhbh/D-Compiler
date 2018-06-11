module Lexer (
    Number.integer,
    Number.signedInteger,
    Symbol.symbol,
    Symbol.parentheses,
    Symbol.semicolon,
    dParser
) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Syntax
import qualified Lexer.Parser as P
import qualified Lexer.Symbol as Symbol
import qualified Lexer.Number as Number
import qualified Lexer.Reserved as Reserved
import qualified Text.Megaparsec.Char.Lexer as L

dParser :: P.Parser Statement
dParser = between P.spaceConsumer eof statement

statement :: P.Parser Statement
statement = f <$> sepBy1 statement' Symbol.semicolon
    where
        f l = if length l == 1 then head l else Sequence l

statement' :: P.Parser Statement
statement' = assignStatement

assignStatement :: P.Parser Statement
assignStatement = do
    var <- Reserved.identifier
    void (Symbol.symbol "=")
    expr <- aExpr
    return (Assign var expr)

aExpr :: P.Parser ArithmeticExpr
aExpr = makeExprParser aTerm aOperators

aTerm :: P.Parser ArithmeticExpr
aTerm = Symbol.parentheses aExpr
{-   <|> Var      <$> identifier
  <|> IntConst <$> integer -}

aOperators :: [[Operator P.Parser ArithmeticExpr]]
aOperators =
    [ [Prefix (Neg <$ Symbol.symbol "-") ]
    , [ InfixL (ArithmeticBinExpr Multiply <$ Symbol.symbol "*")
      , InfixL (ArithmeticBinExpr Divide   <$ Symbol.symbol "/") ]
    , [ InfixL (ArithmeticBinExpr Add      <$ Symbol.symbol "+")
      , InfixL (ArithmeticBinExpr Subtract <$ Symbol.symbol "-") ]
    ]
