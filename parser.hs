module Parser where

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
         <|> try parseCond
         <|> parseCase
         <|> do char '(' -- maybe refactor these, they are kinda messy
                l <- parseList
                char ')'
                return l
         <|> do char '['
                v <- parseVec
                char ']'
                return v

symbol :: Parser Char
symbol = oneOf "!$%+-&|*/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LieVal
parseString = do
    char '"'
    x <- many $ escapedChar <|> noneOf "\""
    char '"'
    return (LieStr x)
    where escapedChar = do
            char '\\'
            x <- oneOf "\\\"nrt"
            return $ case x of
                '\\' -> '\\'
                '\"' -> '\"'
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

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
toDouble (LieInt n)  = fromIntegral n

-- | parses numbers: a + bi
parseComplex :: Parser LieVal
parseComplex = do
    x <- (try parseReal <|> parseInt)
    many space
    char '+'
    many space
    y <- (try parseReal <|> parseInt)
    char 'i'
    return $ LieComplex (toDouble x :+ toDouble y)

-- | parses numbers: bi (only imaginary part)
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

parseWildcardClause :: Parser LieVal
parseWildcardClause =  do
    string "else"
    spaces
    conseq <- parseExpr
    return $ LieList [LieNil, LieAtom "else", conseq, LieAtom "."]

parseClause :: Parser LieVal
parseClause = try parseWildcardClause
           <|> do
                predicate <- parseExpr
                spaces
                string "then"
                spaces
                conseq <- parseExpr
                char '.'
                return $ LieList [predicate, LieAtom "then", conseq, LieAtom "."]

parseCond :: Parser LieVal
parseCond = do
    char '('
    string "cond"
    spaces
    clauses <- sepBy parseClause spaces
    char ')'
    return $ LieList [LieAtom "cond", LieList clauses]

parseCase :: Parser LieVal
parseCase = do
    char '('
    string "case"
    spaces
    expr <- parseExpr
    spaces
    string "of"
    spaces
    clauses <- sepBy parseClause spaces
    char ')'
    return $ LieList [LieAtom "case", expr, LieAtom "of", LieList clauses]