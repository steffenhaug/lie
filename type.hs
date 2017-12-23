module Type (LieVal (LieAtom, LieList, LieVec, LieInt, LieReal,LieComplex, LieStr, LieBool, LieNil)) where

import Numeric
import Data.Complex
import qualified Data.Vector as Vec

data LieVal = LieNil
            | LieAtom String
            | LieList [LieVal]
            | LieVec (Vec.Vector LieVal)
            | LieInt Integer
            | LieReal Double
            | LieComplex (Complex Double)
            | LieStr String
            | LieBool Bool
            deriving Eq

instance Show LieVal where
    show (LieAtom a)         = a
    show (LieInt x)          = show x
    show (LieReal x)         = show x
    show (LieComplex (0:+y)) = (show y) ++ "i"
    show (LieComplex (x:+y)) = (show x) ++ "+" ++ (show y) ++ "i"
    show (LieStr s)          = "\"" ++ s ++ "\""
    show (LieBool b)         = show b
    show LieNil              = "Nil"
    show (LieList l)         = "(" ++ (unwords . map show) l ++ ")"
    show (LieVec v)          = "[" ++ (unwords . map show . Vec.toList) v ++ "]"