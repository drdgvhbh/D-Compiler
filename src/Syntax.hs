module Syntax where

data ArithmeticExpr 
    = ArithmeticBinExpr ArithmeticBinOp ArithmeticExpr ArithmeticExpr
    | Neg ArithmeticExpr
    deriving (Show)
    
data ArithmeticBinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Show)

data Statement 
    = Sequence [Statement]
    | Assign String ArithmeticExpr
    deriving (Show)