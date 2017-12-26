module Type where

import Numeric
import Data.Complex
import Data.IORef
import qualified Data.Vector as Vec
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import System.IO

type Env = IORef [(String, IORef LieVal)]

data LieVal = LieNil
            | LieAtom String
            | LieList [LieVal]
            | LieVec (Vec.Vector LieVal)
            | LieInt Integer
            | LieReal Double
            | LieComplex (Complex Double)
            | LieStr String
            | LieBool Bool
            | LiePrimitive ([LieVal] -> ThrowsException LieVal)
            | LieFunction {params :: [String], body :: LieVal, closure :: Env}
            | LieIOPrimitive ([LieVal] -> IOThrowsException LieVal)
            | LiePort Handle

-- Eq instance for primitives, everything else assumed unequal
instance Eq LieVal where
    LieAtom x == LieAtom y       = x == y
    LieInt x  == LieInt y        = x == y
    LieReal x == LieReal y       = x == y
    LieComplex x == LieComplex y = x == y
    LieStr x == LieStr y         = x == y
    LieBool x == LieBool y       = x == y
    LieVec x == LieVec y         = x == y
    LieList x == LieList y       = x == y
    _ == _                       = False

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
    show (LiePrimitive _)    = "<primitive>"
    show (LieFunction {params = args, body = body, closure = env}) =
        "(λ " ++ unwords args ++ ". <...>)"
    show (LieIOPrimitive _)  = "<io primitive>"
    show (LiePort _)            = "<io port>"

data LieException = ArityException Integer [LieVal]
                  | TypeException String LieVal
                  | BadArgumentException String String [LieVal]
                  | ParserException ParseError
                  | BadFormException String LieVal
                  | NotFunctionException String String
                  | UnboundVariableException String String
                  | AlreadyDefinedException String
                  | Exception String

type IOThrowsException = ExceptT LieException IO
type ThrowsException = Either LieException

liftThrows :: ThrowsException a -> IOThrowsException a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

instance Show LieException where
    show (ArityException expected argv) =
        "Expected " ++ show expected ++ " arguments. " ++
        "Recieved (" ++ (show . length) argv ++ ") arguments: " ++
        unwords (map show argv)
    show (BadArgumentException fn expected argv) =
        "Function " ++ fn ++ " expected arguments of type: " ++ expected ++ ". " ++
        "Recieved arguments: " ++ unwords (map show argv)
    show (TypeException expected found) =
        "Invalid type. Expected " ++ expected ++ ", found " ++ show found
    show (ParserException parErr) =
        "Parser exception at " ++ show parErr
    show (BadFormException message form) =
        message ++ ": " ++ show form
    show (NotFunctionException message function) =
        message ++ ": " ++ show function
    show (UnboundVariableException message symbol) =
        message ++ ": " ++ symbol
    show (AlreadyDefinedException symbol) =
        "Symbol " ++ symbol ++ " is already defined!"

trapException action = catchError action (return . show)

extractValue :: ThrowsException a -> a
extractValue (Right val) = val

makeFunc env params body = return $ LieFunction (map extractAtom params) body env
                           where extractAtom (LieAtom symbol) = symbol