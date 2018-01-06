module Main where

import System.Environment
import System.IO
import System.Directory
import System.Console.Readline
import Control.Monad.Except
import Data.List.Split
import qualified Data.Vector as Vec

import Parser
import Type
import Primitive
import Env

-- this is hacky, i am assuming the executable is called "lie", and dropping
-- four characters ("/lie") from the path...
exe_path = getExecutablePath >>= \x -> return (take (length x - 4) x)
lib_path = exe_path >>= \x -> return (x ++ "/lib")

libraryPath libname = lib_path >>= \x -> return (x ++ "/" ++ libname ++ ".lie")

relativeLibraryPath libname = (libraryPath libname) >>= makeRelativeToCurrentDirectory

apply :: LieVal -> [LieVal] -> IOThrowsException LieVal
apply (LieIOPrimitive func) argv = func argv
apply (LiePrimitive func) argv = liftThrows $ func argv
apply (LieFunction params body closure) argv =
  if num params /= num argv
  then throwError $ ArityException (num params) argv
  else (liftIO $ bindVars closure $ zip params argv) >>= evalBody
  where
    num = toInteger . length
    evalBody env = eval env body

applyProc :: [LieVal] -> IOThrowsException LieVal
applyProc [func, LieList argv] = apply func argv
applyProc [func, LieVec argv]  = apply func (Vec.toList argv)
applyProc (func : argv)        = apply func argv

makePort :: IOMode -> [LieVal] -> IOThrowsException LieVal
makePort mode [LieStr filename] = liftM LiePort $ liftIO $ openFile filename mode

closePort :: [LieVal] -> IOThrowsException LieVal
closePort [LiePort port] = liftIO $ do hClose port
                                       return $ LieBool True
closePort _              = return $ LieBool False

-- | Given a port (handle), reads a Lie procedure *using the Lie parser*.
-- | This is NOT a function to read general files. It only parses Lie procedures.
readProc :: [LieVal] -> IOThrowsException LieVal
readProc []             = readProc [LiePort stdin]
readProc [LiePort port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LieVal] -> IOThrowsException LieVal
writeProc [obj]               = writeProc [obj, LiePort stdout]
writeProc [obj, LiePort port] = liftIO $ hPrint port obj >> (return $ LieBool True)

readContents :: [LieVal] -> IOThrowsException LieVal
readContents [LieStr filename] = liftM LieStr $ liftIO $ readFile filename

load :: String -> IOThrowsException [LieVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

lWrite :: [LieVal] -> IOThrowsException LieVal
lWrite [x]                      = lWrite [x, LiePort stdout]
lWrite [x, LiePort port]        = liftIO $
  hPutStr port (show x) >> (return $ LieBool True)

lWriteLn :: [LieVal] -> IOThrowsException LieVal
lWriteLn [x]                      = lWriteLn [x, LiePort stdout]
lWriteLn [x, LiePort port]        = liftIO $
  hPutStrLn port (show x) >> (return $ LieBool True)

lReadLn :: [LieVal] -> IOThrowsException LieVal
lReadLn []             = lReadLn [LiePort stdin]
lReadLn [LiePort port] = liftM LieStr $ liftIO $ hGetLine port

readAll :: [LieVal] -> IOThrowsException LieVal
readAll [LieStr filename] = liftM (LieVec . Vec.fromList) $ load filename

ioPrimitives :: [(String, [LieVal] -> IOThrowsException LieVal)]
ioPrimitives = [("apply",             applyProc),
                ("open-input-file",   makePort ReadMode),
                ("open-output-file",  makePort WriteMode),
                ("close-input-port",  closePort),
                ("close-output-port", closePort),
                ("read",              lReadLn),
                ("print",             lWrite),
                ("println",           lWriteLn),
                ("read-contents",     readContents),
                ("read-all",          readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
  (flip bindVars $ map (makeFunc LieIOPrimitive) ioPrimitives
   ++ map (makeFunc LiePrimitive) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)
                 


eval :: Env -> LieVal -> IOThrowsException LieVal
-- basic forms evaluate to themselves
eval env val@(LieStr _)     = return val
eval env val@(LieInt _)     = return val
eval env val@(LieReal _)    = return val
eval env val@(LieComplex _) = return val
eval env val@(LieBool _)    = return val
eval env val@(LieNil)       = return val
-- include
eval env (LieList [LieAtom "include", LieStr filename]) = 
    load filename >>= liftM last . mapM (eval env)
-- use
eval env (LieList [LieAtom "use", LieAtom libname]) =
  do path <- liftIO $  relativeLibraryPath libname
     load path >>= liftM last . mapM (eval env)
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
            otherType -> throwError $ TypeException "boolean" otherType
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
eval env (LieList [LieAtom "define", LieAtom var, form]) = eval env form >>= defineVar env var
-- lambda
eval env (LieList [LieAtom "lambda", LieList params, body]) = makeFunc env params body
-- function application
eval env (LieList (fn : argv)) =
  do func    <- eval env fn
     argVals <- mapM (eval env) argv
     apply func argVals
-- if nothing matches, throw exception
eval env badform = throwError $ BadFormException "Unrecognized form" badform

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

help :: String
help = "Press Ctr-D or type \":q\" to exit."

repl :: Env -> IO ()
repl env = do
  maybeLine <- readline "Lie >> "
  case maybeLine of 
    Nothing   -> putStrLn "\nEOF"    >> return () -- EOF / control-d
    Just ":q" -> putStrLn "Goodbye!" >> return ()
    Just ":?" -> putStrLn help       >> repl env
    Just line -> do addHistory line
                    evalAndPrint env line
                    repl env

runOne :: [String] -> IO ()
runOne args =
  do argvector   <- return [("argv", LieVec $ Vec.fromList (map LieStr args))]
     env         <- primitiveBindings >>= flip bindVars argvector
     _ <- runIOThrows $
       liftM show $ eval env (LieList [LieAtom "use", LieAtom "stlib"]) 
     _ <- runIOThrows $
       liftM show $ eval env (LieList [LieAtom "include", LieStr (args !! 0)]) 
     return ()

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  _ <- runIOThrows $
    liftM show $ eval env (LieList [LieAtom "use", LieAtom "stlib"]) 
  repl env

main :: IO ()
main =
  do args <- getArgs
     if null args
       then
       do putStrLn "Lie Interpreter version 0.1. Type \":q\", or press Ctr-D to exit."
          exe_path >>= putStrLn
          lib_path >>= putStrLn
          runRepl
       else runOne $ args
