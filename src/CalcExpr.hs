module CalcExpr (CalcExpr(..), Value(..), calculate) where

import Date

data CalcExpr       = CalcDate Date
                    | CalcRational Rational
                    | CalcPlus CalcExpr CalcExpr
                    | CalcMinus CalcExpr CalcExpr
                    | CalcMultiply CalcExpr CalcExpr
                    | CalcDivide CalcExpr CalcExpr
                    deriving Show

data Value          = ValueDate Date
                    | ValueRational Rational
                    | ValueUndefined
                    deriving Show

genCalculate binaryOp op1 op2 = binaryOp (calculate op1) (calculate op2)

calculate :: CalcExpr -> Value
calculate (CalcDate date)           = ValueDate date
calculate (CalcRational rational)   = ValueRational rational
calculate (CalcPlus op1 op2)       = genCalculate binaryPlus op1 op2
calculate (CalcMinus op1 op2)      = genCalculate binaryMinus op1 op2
calculate (CalcMultiply op1 op2)   = genCalculate binaryMultiply op1 op2
calculate (CalcDivide op1 op2)     = genCalculate binaryDivide op1 op2

binaryPlus op1 op2 = case (op1, op2) of
    (ValueDate d1, ValueDate d2)            -> ValueDate (d1 `plus` d2)
    (ValueRational r1, ValueRational r2)    -> ValueRational (r1 + r2)
    otherwise                               -> ValueUndefined

binaryMinus op1 op2 = case (op1, op2) of
    (ValueDate d1, ValueDate d2)            -> ValueDate (d1 `minus` d2)
    (ValueRational r1, ValueRational r2)    -> ValueRational (r1 - r2)
    otherwise                               -> ValueUndefined

binaryMultiply op1 op2 = case (op1, op2) of
    (ValueDate d, ValueRational r)          -> ValueDate (d `multiply` r)
    (ValueRational r, ValueDate d)          -> ValueDate (d `multiply` r)
    (ValueRational r1, ValueRational r2)    -> ValueRational (r1 * r2)
    otherwise                               -> ValueUndefined

binaryDivide op1 op2 = case (op1, op2) of
    (ValueDate d, ValueRational r)          -> ValueDate (d `divide` r)
    (ValueRational r1, ValueRational r2)    -> ValueRational (r1 / r2)
    otherwise                               -> ValueUndefined