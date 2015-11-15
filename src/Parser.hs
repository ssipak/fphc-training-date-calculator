module Parser (parseCalcExpr) where

import Data.Ratio
import qualified Data.Map.Strict as M

import Text.Parsec
import Text.Parsec.Prim

import Date
import CalcExpr

calcExpr :: Parsec String st CalcExpr
calcExpr =
    try (convertChainToExpr `fmap` parseChain)
    <|> try calcGroup
    <|> calcValue

calcGroup :: Parsec String st CalcExpr
calcGroup = do
    char '('
    spaces
    expr <- calcExpr
    spaces
    char ')'
    return expr

type CalcExprBinaryOperator = CalcExpr -> CalcExpr -> CalcExpr

newtype Operator = Operator { getChar :: Char } deriving (Ord, Eq)

type CalcExprChainLink  = (Operator, CalcExpr)
type CalcExprChain      = (CalcExpr, [CalcExprChainLink])

operators :: [(Char, Integer, CalcExprBinaryOperator)]
operators =
    [ ('+', 0, CalcPlus)
    , ('-', 0, CalcMinus)
    , ('*', 1, CalcMultiply)
    , ('/', 1, CalcDivide)
    ]

operatorConstructorTable :: M.Map Operator CalcExprBinaryOperator
operatorConstructorTable = M.fromList $ map (\(s, _, c) -> (Operator s, c)) operators

operatorConstructor :: Operator -> CalcExprBinaryOperator
operatorConstructor op = M.findWithDefault (error "") op operatorConstructorTable

operatorPrecedenceTable :: M.Map Operator Integer
operatorPrecedenceTable  = M.fromList $ map (\(s, p, _) -> (Operator s, p)) operators

operatorPrecedence :: Operator -> Integer
operatorPrecedence op = M.findWithDefault (error "") op operatorPrecedenceTable

operatorSymbols :: [Char]
operatorSymbols = map (\(s, _, _) -> s) operators

parseChainLink :: Parsec String st (Operator, CalcExpr)
parseChainLink = do
    spaces
    oper <- oneOf operatorSymbols
    spaces
    expr <- calcGroup <|> calcValue
    return (Operator oper, expr)

parseChain :: Parsec String st CalcExprChain
parseChain = do
    e <- calcGroup <|> calcValue
    tail <- many $ try parseChainLink
    return (e, tail)

convertChainToExpr :: CalcExprChain -> CalcExpr
convertChainToExpr (e, []) = e
convertChainToExpr c@(e, links) = convertChainToExpr reducedChain
    where
        linkOpPrec (op, _)  = operatorPrecedence op
        maxPrec             = maximum $ map linkOpPrec links
        reducedChain = reduceChainLinks maxPrec c

reduceChainLinks _ c@(_, []) = c
reduceChainLinks prec c@(e, (op, e2):ls)
    | operatorPrecedence op == prec =
        let constr = operatorConstructor op
        in  reduceChainLinks prec (constr e e2, ls)
    | otherwise =
        let (e2', ls') = reduceChainLinks prec (e2, ls)
        in  (e, (op, e2'):ls')

calcRational :: Parsec String st CalcExpr
calcRational = do
    num <- integer
    denum <- option 1 $ try (char '%' >> natural)
    return $ CalcRational (num % denum)

calcDate :: Parsec String st CalcExpr
calcDate = do
    sign <- sign
    days <- option 0 $ try $ do
        d <- natural
        char '.'
        return d
    hours <- natural
    char ':'
    minutes <- natural
    seconds <- option 0 $ try $ do
        char ':'
        natural
    return . CalcDate $
        let absDate = fromDateComponents (days, hours, minutes, seconds)
        in case sign of
                Minus -> Date 0 `minus` absDate
                Plus -> absDate

calcValue = try calcDate <|> calcRational

natural :: Parsec String st Integer
natural = read `fmap` many1 digit

integer :: Parsec String st Integer
integer = do
    sign <- option '+' (char '-' <|> char '+')
    digits <- many1 digit
    return . read $ case sign of
                        '+' -> digits
                        '-' -> sign:digits

data Sign = Plus | Minus
sign :: Parsec String st Sign
sign = option Plus $ (char '-' >> return Minus) <|> (char '+' >> return Plus)

parseCalcExpr :: String -> Either ParseError CalcExpr
parseCalcExpr = parse (spaces >> calcExpr) "Input string"