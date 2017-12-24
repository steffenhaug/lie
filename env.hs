module Env where

import Data.IORef
import Control.Monad.Except

import Type
import Primitive

nullEnv :: IO Env
nullEnv = newIORef []

runIOThrows :: IOThrowsException String -> IO String
runIOThrows action = runExceptT (trapException action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsException LieVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVariableException "Tried to lookup unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LieVal -> IOThrowsException LieVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVariableException "Tried to mutate unbound variable" var)
                                     (liftIO . (flip writeIORef value))
                                     (lookup var env)
                             return value

defineVar :: Env -> String -> LieVal -> IOThrowsException LieVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
                 where makePrimitiveFunc (var, func) = (var, LiePrimitive func)

bindVars :: Env -> [(String, LieVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
        where extendEnv bindings env  = liftM (++ env) (mapM addBinding bindings)
              addBinding (var, value) = do ref <- newIORef value
                                           return (var, ref)