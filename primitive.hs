module Primitive where

import Numeric
import Data.Complex
import Control.Monad.Except
import qualified Data.Vector as Vec

import Type
import Exception

primitives :: [(String, [LieVal] -> ThrowsException LieVal)]
primitives = [("id", unaryOp (\x -> x)),
              ("+", foldVariadic  lieAdd),
              ("-", foldVariadic' lieSub (LieInt 0)),
              ("*", foldVariadic  lieMul),
              ("/", foldVariadic' lieRealDiv (LieInt 1)),
              ("div", foldVariadic lieDiv),
              ("mod", foldVariadic lieMod),
              ("im", unaryOp lieIm),
              ("re", unaryOp lieRe),
              ("conj", lieConj),
              ("int?", unaryOp intp),
              ("str?", unaryOp strp),
              ("real?", unaryOp realp),
              ("complex?", unaryOp complexp),
              ("atom?", unaryOp atomp),
              ("vec?", unaryOp vecp)]

apply :: String -> [LieVal] -> ThrowsException LieVal
apply fn argv = maybe (throwError $ NotFunctionException "Unrecognized primitive function" fn)
                      ($ argv)
                      (lookup fn primitives)

unaryOp :: (LieVal -> LieVal) -> [LieVal] -> ThrowsException LieVal
unaryOp fn [] = throwError $ ArityException 1 []
unaryOp fn [x] = return $ fn x

lieConj :: [LieVal] -> ThrowsException LieVal
lieConj [] = throwError $ ArityException 1 []
lieConj v = return . LieVec $ (foldl1 (Vec.++) . map extractVector) v
            where 
                extractVector (LieVec v) = v

-- | numeric binary operations on lie types, with a special case for arity=1
foldVariadic' :: (LieVal -> LieVal -> LieVal) -> LieVal -> [LieVal] -> ThrowsException LieVal
foldVariadic' fn a [] = throwError $ ArityException 1 []
foldVariadic' fn a (x:[]) = return $ fn a x

-- | standard numeric binary operation on LieTypes
foldVariadic :: (LieVal -> LieVal -> LieVal) -> [LieVal] -> ThrowsException LieVal
foldVariadic fn [] = throwError $ ArityException 2 []
foldVariadic fn single@[_] = throwError $ ArityException 2 single
foldVariadic fn argv =return $ foldl1 fn argv

-- this needs to be LieVal -> LieVal -> ThrowsException LieVal
-- needs to throw exception when args are not numbers
lieAdd :: LieVal -> LieVal -> LieVal
lieAdd (LieInt x) (LieInt y)   = LieInt (x + y)
lieAdd (LieInt x) (LieReal y)  = LieReal (fromIntegral x + y)
lieAdd (LieReal x) (LieInt y)  = LieReal (x + fromIntegral y)
lieAdd (LieReal x) (LieReal y) = LieReal (x + y)

lieSub :: LieVal -> LieVal -> LieVal
lieSub (LieInt x) (LieInt y)   = LieInt (x - y)
lieSub (LieInt x) (LieReal y)  = LieReal (fromIntegral x - y)
lieSub (LieReal x) (LieInt y)  = LieReal (x - fromIntegral y)
lieSub (LieReal x) (LieReal y) = LieReal (x - y)


lieMul :: LieVal -> LieVal -> LieVal
lieMul (LieInt x) (LieInt y)   = LieInt (x * y)
lieMul (LieInt x) (LieReal y)  = LieReal (fromIntegral x * y)
lieMul (LieReal x) (LieInt y)  = LieReal (x * fromIntegral y)
lieMul (LieReal x) (LieReal y) = LieReal (x * y)


-- | "RealDiv", because it divides real numbers, as opposed to "Div"
lieRealDiv :: LieVal -> LieVal -> LieVal
lieRealDiv (LieInt x) (LieInt y)   = LieReal (fromIntegral x / fromIntegral y)
lieRealDiv (LieInt x) (LieReal y)  = LieReal (fromIntegral x / y)
lieRealDiv (LieReal x) (LieInt y)  = LieReal (x / fromIntegral y)
lieRealDiv (LieReal x) (LieReal y) = LieReal (x / y)


lieDiv :: LieVal -> LieVal -> LieVal
lieDiv (LieInt x) (LieInt y) = LieInt (div x y)


lieMod :: LieVal -> LieVal -> LieVal
lieMod (LieInt x) (LieInt y) = LieInt (mod x y)


lieIm :: LieVal -> LieVal
lieIm (LieComplex (a :+ b)) = LieReal b


lieRe :: LieVal -> LieVal
lieRe (LieComplex (a :+ b)) = LieReal a
