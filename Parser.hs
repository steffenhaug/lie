module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Complex
import qualified Data.Vector as Vec
import Control.Monad.Except
import System.IO

import Type
import Primitive

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr (spaces <|> eof <|> skipMany (char '\n')))

symbol :: Parser Char
symbol = oneOf "!$%+-&|*/:'<=>?@^_~"

-- |Language definition for Lie
lieDef :: LanguageDef ()
lieDef 
  = emptyDef    
  { P.commentLine    = "--"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol
  , P.reservedNames  = []
  , P.caseSensitive  = True
} 
  
parseExpr :: Parser LieVal
parseExpr = do exp <-  parseNumber
                   <|> parseAtom
                   <|> parseString
                   <|> do char '('
                          l <- parseListLike
                          char ')'
                          return l
                   <|> do char '['
                          l <- parseVec
                          char ']'
                          return l
                   <?> "expression"
               return exp

readOrThrow :: Parser a -> String -> ThrowsException a
readOrThrow parser input = case parse parser "lie lisp" input of
    Left err  -> throwError $ ParserException err
    Right val -> return val

parseListLike :: Parser LieVal
parseListLike = do
    l <-  try parseCond
      <|> try parseCase
      <|> try parseLet
      <|> try parseLambda
      <|> try parseFunction
      <|> parseList
      <?> "List-like"
    return l

spaces :: Parser ()
spaces = skipMany1 space

comment :: Parser ()
comment = do string "--"
             skipMany (noneOf "\r\n")
         <?> "comment"

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
                return $ LieList [predicate, LieAtom "then", conseq, LieAtom "."]

parseCond :: Parser LieVal
parseCond = do
    string "cond"
    spaces
    clauses <- sepBy parseClause (do {char '.'; many space})
    return $ LieList [LieAtom "cond", LieList clauses]

parseCase :: Parser LieVal
parseCase = do
    string "case"
    spaces
    expr <- parseExpr
    spaces
    string "of"
    spaces
    clauses <- sepBy parseClause (do {char '.'; many space})
    return $ LieList [LieAtom "case", expr, LieAtom "of", LieList clauses]

-- | parser for function bodies. not that this parser assumes it
-- | is parsing a list, so we could end up with singleton function
-- | bodies, like (x), which would not evaluate properly. this case
-- | is accounted for by "extractSingleton".
parseFunctionBody :: Parser LieVal
parseFunctionBody = 
    do
        l <- parseListLike
        return $ extractSingleton l
    where
        -- extract the value from a singleton list
        extractSingleton li@(LieList l) = case length l of
            1 -> head l
            _ -> li

parseLambda :: Parser LieVal
parseLambda = do
    string "lambda" <|> string "λ"
    spaces
    argv <- sepBy parseAtom spaces
    char '.'
    spaces
    expr <- parseFunctionBody
    return $ LieList [LieAtom "lambda", LieList argv, expr]

parseFunction :: Parser LieVal
parseFunction = do
    string "function"
    spaces
    symbol <- parseAtom
    spaces
    argv <- sepBy parseAtom spaces
    char '.'
    spaces
    expr <- parseFunctionBody
    return $ LieList [LieAtom "define", symbol, LieList [LieAtom "lambda", LieList argv, expr]]

parseLetClause :: Parser (LieVal, LieVal)
parseLetClause = do
    key <- parseAtom
    spaces
    string "<-"
    spaces
    value <- parseExpr
    return $ (key, value)

parseLet :: Parser LieVal
parseLet = do
    string "let"
    spaces
    bindings <- endBy (try parseLetClause) (do {char '.'; many space})
    string "in"
    spaces
    expr <- parseFunctionBody
    return $ convertToLambda bindings expr
    where
        convertToLambda bindings' expr' =
            let (keys, vals) = split bindings' 
            in LieList (LieList [LieAtom "lambda", LieList keys, expr']:vals)
            where split l = ([fst x | x <- l], [snd x | x <- l])
