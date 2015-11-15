module Date (Date(Date), dateComponents, fromDateComponents, plus, minus, multiply, divide) where

data Date = Date Integer

dateComponents :: Date -> (Integer, Integer, Integer, Integer, Integer)
dateComponents (Date secs) = (sign, d, h, m, s)
    where
        sign = signum secs
        (mins, s) = divMod (abs secs) 60
        (hours, m) = divMod mins 60
        (d, h) = divMod hours 24

fromDateComponents :: (Integer, Integer, Integer, Integer) -> Date
fromDateComponents (d, h, m, s) = Date (s + 60*(m + 60*(h + 24*d)))

pad2 :: Integer -> String
pad2 i  | i >= 0 && l <= 1 = "0" ++ s
        | otherwise = s
        where
            s = show i
            l = length s


instance Show Date where
    show date = case (dateComponents date) of
        (sign, 0, h, m, 0) -> showSign sign ++ show h ++ ":" ++ pad2 m
        (sign, d, h, m, s) -> showSign sign ++ show d ++ "." ++ pad2 h ++ ":" ++ pad2 m ++ ":" ++ pad2 s
        where
            showSign sign | sign >= 0 = ""
                          | otherwise = "-"

plus :: Date -> Date -> Date
plus (Date secs1) (Date secs2) = Date (secs1 + secs2)
minus :: Date -> Date -> Date
minus (Date secs1) (Date secs2) = Date (secs1 - secs2)
multiply :: Date -> Rational -> Date
multiply (Date secs) r = Date $ floor $ (fromIntegral secs) * r
divide :: Date -> Rational -> Date
divide (Date secs) r = Date $ floor $ (fromIntegral secs) / r