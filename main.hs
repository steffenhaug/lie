module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except


import Parser
import Type
import Primitive
import Exception

eval :: LieVal -> ThrowsException LieVal
eval val@(LieStr _)                = return val
eval val@(LieInt _)                = return val
eval val@(LieReal _)               = return val
eval val@(LieComplex _)            = return val
eval val@(LieBool _)               = return val
eval (LieList (LieAtom fn : argv)) = mapM eval argv >>= apply fn
eval (LieVec v)                    = mapM eval v >>= return . LieVec
eval badform = throwError $ BadFormException "Unrecognized form" badform

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled