module Primitive where

import Numeric
import Data.Complex
import Control.Monad.Except
import qualified Data.Vector as Vec
import System.IO

import Type

primitives :: [(String, [LieVal] -> ThrowsException LieVal)]
primitives = [("+",         foldV  lieAdd),
              ("-",         foldV' lieSub (LieInt 0)),
              ("*",         foldV  lieMul),
              ("/",         foldV' lieRealDiv (LieInt 1)),
              ("=",         binaryOp lieEq),
              ("!=",        binaryOp lieNeq),
              ("<",         binaryOp lieLt),
              (">",         binaryOp lieGt),
              ("<=",        binaryOp lieLeq),
              (">=",        binaryOp lieGeq),
              ("and",       binaryOp lieAnd),
              ("or",        binaryOp lieOr),
              ("xor",       binaryOp lieXor),
              ("div",       foldV lieDiv),
              ("mod",       foldV lieMod),
              ("im",        unaryOp lieIm),
              ("re",        unaryOp lieRe),
              ("conj",      lieConj),
              ("head",      lieHead),
              ("tail",      lieTail),
              ("init",      lieInit),
              ("get",       lieGet),
              ("length",    lieLen),
              ("int?",      unaryOp intp),
              ("str?",      unaryOp strp),
              ("real?",     unaryOp realp),
              ("complex?",  unaryOp complexp),
              ("atom?",     unaryOp atomp),
              ("vec?",      unaryOp vecp)]

unaryOp :: (LieVal -> ThrowsException LieVal) -> [LieVal] -> ThrowsException LieVal
unaryOp fn []  = throwError $ ArityException 1 []
unaryOp fn [x] = fn x
unaryOp fn v   = throwError $ ArityException 1 v

binaryOp :: (LieVal -> LieVal -> ThrowsException LieVal) -> [LieVal]
         -> ThrowsException LieVal
binaryOp fn []     = throwError $ ArityException 2 []
binaryOp fn [x]    = throwError $ ArityException 2 [x]
binaryOp fn [x, y] = fn x y
binaryOp fn v      = throwError $ ArityException 2 v

lieLen :: [LieVal] -> ThrowsException LieVal
lieLen [] = throwError $ ArityException 1 []
lieLen (LieVec v:[]) = return $ LieInt . fromIntegral $ Vec.length v
lieLen argv = throwError $ ArityException 1 argv

lieConj :: [LieVal] -> ThrowsException LieVal
lieConj [] = throwError $ ArityException 1 []
lieConj v = return . LieVec $ (foldl1 (Vec.++) . map extractVector) v
            where 
                extractVector (LieVec v) = v

-- do error handling when index oob. Vec.!? -> Maybe LieVal
lieGet :: [LieVal] -> ThrowsException LieVal
lieGet [] = throwError $ ArityException 1 []
lieGet (LieInt i : LieVec v : []) = return $ v Vec.! (fromInteger i)
lieGet (LieInt i : LieInt j : LieVec v : []) = return $
  LieVec $ Vec.slice (fromInteger i) (fromInteger j) v
lieGet argv = throwError $ ArityException 3 argv

lieHead :: [LieVal] -> ThrowsException LieVal
lieHead [] = throwError $ ArityException 1 []
lieHead (LieVec v:[]) = return $ case Vec.length v of
                                    0 -> LieNil
                                    _ -> Vec.head v
lieHead argv = throwError $ ArityException 1 argv

lieTail :: [LieVal] -> ThrowsException LieVal
lieTail [] = throwError $ ArityException 1 []
lieTail (LieVec v:[]) = return $ case Vec.length v of
                                    0 -> LieNil
                                    _ -> LieVec (Vec.tail v)
lieTail argv@(_:_) = throwError $ ArityException 1 argv

lieInit :: [LieVal] -> ThrowsException LieVal
lieInit [] = throwError $ ArityException 1 []
lieInit (LieVec v:[]) = return $ case Vec.length v of
                                    0 -> LieNil
                                    _ -> LieVec (Vec.init v)
lieInit argv@(_:_) = throwError $ ArityException 1 argv

lieLast :: [LieVal] -> ThrowsException LieVal
lieLast [] = throwError $ ArityException 1 []
lieLast (LieVec v:[]) = return $ Vec.last v
lieLast argv@(_:_) = throwError $ ArityException 1 argv

foldV' :: (LieVal -> LieVal -> ThrowsException LieVal) -> LieVal -> [LieVal]
       -> ThrowsException LieVal
foldV' fn a [] = throwError $ ArityException 1 []
foldV' fn a [x] = fn a x
foldV' fn a l@(x:xs) = foldV fn l

foldV :: (LieVal -> LieVal -> ThrowsException LieVal) -> [LieVal] -> ThrowsException LieVal
foldV fn []  = throwError $ ArityException 2 []
foldV fn [x] = throwError $ ArityException 2 [x]
foldV fn (arg:args) = foldM fn arg args

lieAdd :: LieVal -> LieVal -> ThrowsException LieVal
lieAdd (LieInt x) (LieInt y)   = return $ LieInt (x + y)
lieAdd (LieInt x) (LieReal y)  = return $ LieReal (fromIntegral x + y)
lieAdd (LieReal x) (LieInt y)  = return $ LieReal (x + fromIntegral y)
lieAdd (LieReal x) (LieReal y) = return $ LieReal (x + y)
lieAdd a b = throwError $ BadArgumentException "+" "number, number" [a, b]

lieSub :: LieVal -> LieVal -> ThrowsException LieVal
lieSub (LieInt x) (LieInt y)   = return $ LieInt (x - y)
lieSub (LieInt x) (LieReal y)  = return $ LieReal (fromIntegral x - y)
lieSub (LieReal x) (LieInt y)  = return $ LieReal (x - fromIntegral y)
lieSub (LieReal x) (LieReal y) = return $ LieReal (x - y)
lieSub a b = throwError $ BadArgumentException "-" "number, number" [a, b]

lieMul :: LieVal -> LieVal -> ThrowsException LieVal
lieMul (LieInt x) (LieInt y)   = return $ LieInt (x * y)
lieMul (LieInt x) (LieReal y)  = return $ LieReal (fromIntegral x * y)
lieMul (LieReal x) (LieInt y)  = return $ LieReal (x * fromIntegral y)
lieMul (LieReal x) (LieReal y) = return $ LieReal (x * y)
lieMul a b = throwError $ BadArgumentException "*" "number, number" [a, b]

-- | "RealDiv", because it divides real numbers, as opposed to "Div"
lieRealDiv :: LieVal -> LieVal -> ThrowsException LieVal
lieRealDiv (LieInt x) (LieInt y)   = return $ LieReal (fromIntegral x / fromIntegral y)
lieRealDiv (LieInt x) (LieReal y)  = return $ LieReal (fromIntegral x / y)
lieRealDiv (LieReal x) (LieInt y)  = return $ LieReal (x / fromIntegral y)
lieRealDiv (LieReal x) (LieReal y) = return $ LieReal (x / y)
lieRealDiv a b = throwError $ BadArgumentException "/" "number, number" [a, b]

lieDiv :: LieVal -> LieVal -> ThrowsException LieVal
lieDiv (LieInt x) (LieInt y) = return $ LieInt (div x y)
lieDiv a b = throwError $ BadArgumentException "div" "integer, integer" [a, b]

lieMod :: LieVal -> LieVal -> ThrowsException LieVal
lieMod (LieInt x) (LieInt y) = return $ LieInt (mod x y)
lieMod a b = throwError $ BadArgumentException "mod" "integer, integer" [a, b]

lieIm :: LieVal -> ThrowsException LieVal
lieIm (LieComplex (a :+ b)) = return $ LieReal b
lieIm a = throwError $ BadArgumentException "im" "complex number, complex number" [a]

lieRe :: LieVal -> ThrowsException LieVal
lieRe (LieComplex (a :+ b)) = return $ LieReal a
lieRe a = throwError $ BadArgumentException "re" "complex number, complex number" [a]

lieEq :: LieVal -> LieVal -> ThrowsException LieVal
lieEq (LieInt i) (LieInt j)         = return $ LieBool (i == j)
lieEq (LieInt i) (LieReal j)        = return $ LieBool (fromIntegral i == j)
lieEq (LieReal i) (LieInt j)        = return $ LieBool (i == fromIntegral j)
lieEq (LieReal i) (LieReal j)       = return $ LieBool (i == j)
lieEq (LieComplex i) (LieComplex j) = return $ LieBool (i == j)
lieEq (LieStr i) (LieStr j)         = return $ LieBool (i == j)
lieEq (LieVec i) (LieVec j)         = return $ LieBool (i == j)
lieEq (LieBool i) (LieBool j)       = return $ LieBool (i == j)
lieEq _ _ = return $ LieBool False

lieNeq :: LieVal -> LieVal -> ThrowsException LieVal
lieNeq (LieInt i) (LieInt j)         = return $ LieBool (i /= j)
lieNeq (LieInt i) (LieReal j)        = return $ LieBool (fromIntegral i /= j)
lieNeq (LieReal i) (LieInt j)        = return $ LieBool (i /= fromIntegral j)
lieNeq (LieReal i) (LieReal j)       = return $ LieBool (i /= j)
lieNeq (LieComplex i) (LieComplex j) = return $ LieBool (i /= j)
lieNeq (LieStr i) (LieStr j)         = return $ LieBool (i /= j)
lieNeq (LieVec i) (LieVec j)         = return $ LieBool (i /= j)
lieNeq (LieBool i) (LieBool j)       = return $ LieBool (i /= j)
lieNeq _ _ = return $ LieBool True

lieGeq :: LieVal -> LieVal -> ThrowsException LieVal
lieGeq (LieInt i) (LieInt j)   = return $ LieBool (i >= j)
lieGeq (LieInt i) (LieReal j)  = return $ LieBool (fromIntegral i >= j)
lieGeq (LieReal i) (LieInt j)  = return $ LieBool (i >= fromIntegral j)
lieGeq (LieReal i) (LieReal j) = return $ LieBool (i >= j)
lieGeq a b = throwError $ BadArgumentException ">=" "number, number" [a, b]

lieLeq :: LieVal -> LieVal -> ThrowsException LieVal 
lieLeq (LieInt i) (LieInt j)   = return $ LieBool (i <= j)
lieLeq (LieInt i) (LieReal j)  = return $ LieBool (fromIntegral i <= j)
lieLeq (LieReal i) (LieInt j)  = return $ LieBool (i <= fromIntegral j)
lieLeq (LieReal i) (LieReal j) = return $ LieBool (i <= j)
lieLeq a b = throwError $ BadArgumentException "<=" "number, number" [a, b]

lieGt :: LieVal -> LieVal -> ThrowsException LieVal
lieGt (LieInt i) (LieInt j)   = return $ LieBool (i > j)
lieGt (LieInt i) (LieReal j)  = return $ LieBool (fromIntegral i > j)
lieGt (LieReal i) (LieInt j)  = return $ LieBool (i > fromIntegral j)
lieGt (LieReal i) (LieReal j) = return $ LieBool (i > j)
lieGt a b = throwError $ BadArgumentException ">" "number, number" [a, b]

lieLt :: LieVal -> LieVal -> ThrowsException LieVal
lieLt (LieInt i) (LieInt j)   = return $ LieBool (i < j)
lieLt (LieInt i) (LieReal j)  = return $ LieBool (fromIntegral i < j)
lieLt (LieReal i) (LieInt j)  = return $ LieBool (i < fromIntegral j)
lieLt (LieReal i) (LieReal j) = return $ LieBool (i < j)
lieLt a b = throwError $ BadArgumentException "<" "number, number" [a, b]

lieAnd :: LieVal -> LieVal -> ThrowsException LieVal
lieAnd (LieBool i) (LieBool j) = return $ LieBool (i && j)
lieAnd a b = throwError $ BadArgumentException "and" "boolean, boolean" [a, b]

lieOr :: LieVal -> LieVal -> ThrowsException LieVal
lieOr (LieBool i) (LieBool j) = return $ LieBool (i || j)
lieOr a b = throwError $ BadArgumentException "or" "boolean, boolean" [a, b]

lieXor :: LieVal -> LieVal -> ThrowsException LieVal
lieXor (LieBool i) (LieBool j) = return $ LieBool $ (i || j) && not (i && j)
lieXor a b = throwError $ BadArgumentException "xor" "boolean, boolean" [a, b]

atomp, intp, realp, complexp, strp, boolp, vecp :: LieVal -> ThrowsException LieVal
atomp (LieAtom _)       = return (LieBool True)
atomp _                 = return (LieBool False)
intp (LieInt _)         = return (LieBool True)
intp _                  = return (LieBool False)
realp (LieReal _)       = return (LieBool True)
realp _                 = return (LieBool False)
complexp (LieComplex _) = return (LieBool True)
complexp _              = return (LieBool False)
strp (LieStr _)         = return (LieBool True)
strp _                  = return (LieBool False)
boolp (LieBool _)       = return (LieBool True)
boolp _                 = return (LieBool False)
vecp (LieVec _)         = return (LieBool True)
vecp _                  = return (LieBool False)
