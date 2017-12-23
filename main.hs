module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except

import Parser
import Type
import Primitive
import Exception

eval :: LieVal -> ThrowsException LieVal
eval val@(LieStr _)     = return val
eval val@(LieInt _)     = return val
eval val@(LieReal _)    = return val
eval val@(LieComplex _) = return val
eval val@(LieBool _)    = return val
eval val@(LieNil)       = return val
eval (LieVec v)         = mapM eval v >>= return . LieVec
eval (LieList [LieAtom "if", pred, LieAtom "then", conseq, LieAtom "else", alt]) = 
    do result <- eval pred
       case result of
            LieBool True  -> eval conseq
            LieBool False -> eval alt
            a -> throwError $ TypeException "boolean" a
eval (LieList [LieAtom "unless", pred, LieAtom "then", conseq, LieAtom "else", alt]) = 
    do result <- eval pred
       case result of
            LieBool False -> eval conseq
            LieBool True  -> eval alt
            a -> throwError $ TypeException "boolean" a
eval (LieList [LieAtom "cond", LieList clauses]) = evalClauses clauses
        where evalClauses [] = return LieNil
              evalClauses (LieList [pred, _, conseq, _]:cs) =
                do result <- eval pred
                   case result of
                        LieBool True  -> eval conseq
                        LieBool False -> evalClauses cs
                        a -> throwError $ TypeException "boolean" a
eval (LieList (LieAtom fn : argv)) = mapM eval argv >>= apply fn
eval badform = throwError $ BadFormException "Unrecognized form" badform

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapException evaled