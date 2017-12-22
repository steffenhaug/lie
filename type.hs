module Type (LieVal (LieAtom, LieList, LieVec, LieInt, LieReal,LieComplex, LieStr, LieBool, LieNil),
             atomp, intp, realp, complexp, strp, boolp, vecp) where

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

atomp, intp, realp, complexp, strp, boolp, vecp :: LieVal -> LieVal
atomp (LieAtom _)   = LieBool True
atomp _             = LieBool False
intp (LieInt _)     = LieBool True
intp _              = LieBool False
realp (LieReal _)   = LieBool True
realp _             = LieBool False
complexp (LieComplex _) 
                    = LieBool True
complexp _          = LieBool False
strp (LieStr _)     = LieBool True
strp _              = LieBool False
boolp (LieBool _)   = LieBool True
boolp _             = LieBool False
vecp (LieVec _)     = LieBool True
vecp _              = LieBool False