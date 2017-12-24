module Main where

import System.Environment
import System.IO
import System.Console.Readline
import Control.Monad.Except

import Parser
import Type
import Primitive
import Env

apply :: LieVal -> [LieVal] -> IOThrowsException LieVal
apply (LiePrimitive func) argv = liftThrows $ func argv
apply (LieFunction params body closure) argv =
    if num params /= num argv
        then throwError $ ArityException (num params) argv
        else (liftIO $ bindVars closure $ zip params argv) >>= evalBody
    where
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body

eval :: Env -> LieVal -> IOThrowsException LieVal
-- basic forms evaluate to themselves
eval env val@(LieStr _)     = return val
eval env val@(LieInt _)     = return val
eval env val@(LieReal _)    = return val
eval env val@(LieComplex _) = return val
eval env val@(LieBool _)    = return val
eval env val@(LieNil)       = return val

-- variable lookup
eval env (LieAtom symbol) = getVar env symbol

-- vector
eval env (LieVec v) = mapM (eval env) v >>= return . LieVec

-- if-then-else
eval env (LieList [LieAtom "if", pred, LieAtom "then", conseq, LieAtom "else", alt]) = 
    do result <- eval env pred
       case result of
            LieBool True  -> eval env conseq
            LieBool False -> eval env alt
            a -> throwError $ TypeException "boolean" a

-- unless-then-else
eval env (LieList [LieAtom "unless", pred, LieAtom "then", conseq, LieAtom "else", alt]) = 
    do result <- eval env pred
       case result of
            LieBool False -> eval env conseq
            LieBool True  -> eval env alt
            a -> throwError $ TypeException "boolean" a

-- cond
eval env (LieList [LieAtom "cond", LieList clauses]) = evalClauses clauses
        where evalClauses [] = return LieNil
              evalClauses (LieList [_, LieAtom "else", conseq, _]:cs) = eval env conseq
              evalClauses (LieList [pred, _, conseq, _]:cs) =
                do result <- eval env pred
                   case result of
                        LieBool True  -> eval env conseq
                        LieBool False -> evalClauses cs
                        a -> throwError $ TypeException "boolean" a

-- case-of
eval env (LieList [LieAtom "case", expr, LieAtom "of", LieList clauses]) =
    do result <- eval env expr
       evalClauses result clauses
       where evalClauses _ [] = return LieNil
             evalClauses _ (LieList [_, LieAtom "else", conseq, _]:cs) = eval env conseq
             evalClauses r (LieList [pred, _, conseq, _]:cs) =
                do r' <- eval env pred
                   case r' == r of
                        True  -> eval env conseq
                        False -> evalClauses r cs

-- mutate a defined symbol
eval env (LieList [LieAtom "set!", LieAtom var, form]) =
    eval env form >>= setVar env var
                
-- symbol definition
eval env (LieList [LieAtom "def", LieAtom var, form]) = eval env form >>= defineVar env var

-- lambda
eval env (LieList (LieAtom "lambda" : LieList params : body)) = makeFunc env params body

-- primitive function application
eval env (LieList (fn : argv)) = do
    func <- eval env fn
    argVals <- mapM (eval env) argv
    apply func argVals

eval env badform = throwError $ BadFormException "Unrecognized form" badform

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

repl :: Env -> IO ()
repl env = do
    maybeLine <- readline "Lie >> "
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line   -> do addHistory line
                          evalAndPrint env line
                          repl env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= repl

main :: IO ()
main = do args <- getArgs
          case length args of
                  0 -> do
                    putStrLn "Lie Interpreter version 0.1. Type \"exit\", or press Ctr-D to exit."
                    runRepl
                  1 -> runOne $ args !! 0
                  otherwise -> putStrLn "Program takes only 0 or 1 argument"
