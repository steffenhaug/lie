module Parser (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Complex
import qualified Data.Vector as Vec
import Control.Monad.Except

import Type
import Primitive
import Exception

readExpr :: String -> ThrowsException LieVal
readExpr input = case parse parseExpr ("Parsing Expression " ++ input) input of
    Left err  -> throwError $ ParserException err
    Right val -> return val

parseExpr :: Parser LieVal
parseExpr =  parseNumber
         <|> parseAtom
         <|> parseString
         <|> do char '(' -- maybe refactor these, they are kinda messy
                l <- parseList
                char ')'
                return l
         <|> do char '['
                v <- parseVec
                char ']'
                return v

-- TODO: make this: v <- delim '[' parser ']'

symbol :: Parser Char
symbol = oneOf "!$%+-&|*/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> '\\'
        '\"' -> '\"'
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LieVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\""
    char '"'
    return (LieStr x)

parseAtom :: Parser LieVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "True"  -> LieBool True
        "False" -> LieBool False
        "Nil"   -> LieNil
        _       -> LieAtom atom

-- | umbrella parser to hide the fact that the number parsing is super ugly
-- | the number parser should be left-factored, then one could avoid "try"
parseNumber :: Parser LieVal
parseNumber =  try parseComplex
           <|> try parseComplex'
           <|> try parseReal
           <|> try parseInt

parseInt :: Parser LieVal
parseInt = do
    s <- many (oneOf "-")
    x <- many1 digit
    return $ (LieInt . read) (s ++ x)

parseReal :: Parser LieVal
parseReal = do
    s <- many (oneOf "-")    
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ LieReal $ (fst . head) $ readFloat (s ++ x ++ "." ++ y)

toDouble :: LieVal -> Double
toDouble (LieReal f) = realToFrac f
toDouble (LieInt n) = fromIntegral n

-- | parses numbers a + bi
parseComplex :: Parser LieVal
parseComplex = do
    x <- (try parseReal <|> parseInt)
    many space
    char '+'
    many space
    y <- (try parseReal <|> parseInt)
    char 'i'
    return $ LieComplex (toDouble x :+ toDouble y)

-- | parses numbers bi (only imaginary part)
parseComplex' :: Parser LieVal
parseComplex' = do
    x <- (try parseReal <|> parseInt)
    char 'i'
    return $ LieComplex (0 :+ toDouble x)

parseList :: Parser LieVal
parseList = do
    l <- sepBy parseExpr spaces
    return $ LieList l

parseVec :: Parser LieVal
parseVec = do
    l <- sepBy parseExpr spaces
    return $ LieVec $ Vec.fromList l