-- | Main entry point to the application.
module Main where

import Parser
import CalcExpr
import Date

-- | The main entry point.
main :: IO ()
main = do
    line <- getLine
    case parseCalcExpr line of
        Left error -> print error
        Right expr -> do
            print $ expr
            print $ calculate expr
    main