module Exception (LieException (ArityException, TypeException, BadArgumentException, ParserException, BadFormException, NotFunctionException, UnboundVariableException, Exception),
                  ThrowsException, trapError, extractValue) where
                    
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

import Type

data LieException = ArityException Integer [LieVal]
                  | TypeException String LieVal
                  | BadArgumentException String String [LieVal]
                  | ParserException ParseError
                  | BadFormException String LieVal
                  | NotFunctionException String String
                  | UnboundVariableException String String
                  | Exception String

instance Show LieException where
    show (ArityException expected argv) =
        "Expected " ++ show expected ++ " arguments. " ++
        "Recieved (" ++ (show . length) argv ++ ") arguments: " ++
        unwords (map show argv)
    show (BadArgumentException fn expected argv) =
        "Function " ++ fn ++ " expected argument of type " ++ expected ++ ". " ++
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

type ThrowsException = Either LieException

trapError action = catchError action (return . show)

extractValue :: ThrowsException a -> a
extractValue (Right val) = val