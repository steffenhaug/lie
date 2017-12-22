import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Vector as Vec

data NumericType = I Integer | F Double deriving (Show)

numAdd :: NumericType -> NumericType -> NumericType
numAdd (I i) (I j) = I (i + j)
numAdd (F i) (F j) = F (i + j)
numAdd (I i) (F j) = F (fromIntegral i + j)
numAdd (F i) (I j) = F (i + fromIntegral j)


-- lambda x, y: x + y
-- lambda x y -> (+ x y)

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser [String]
parseList = do
    l <- sepBy (many1 (letter <|> digit)) spaces
    return l

t :: Parser String
t = do
    s <- string "->"
    return s

lambdaParser :: Parser String
lambdaParser = do
    (string "lambda") <|> (string "λ")
    spaces
    argv <- parseList
    char '.'
    spaces
    string "->"
    spaces
    char '('
    ast <- parseList
    char ')'
    return $ unwords argv ++ ", " ++ unwords ast

lpar :: Parser String
lpar = do
    s <- string "s"
    many spaces
    t <- string "t"
    return $ s ++ t

p :: String -> String
p s = case parse lambdaParser "fuck elsys" s of
                Right st -> st
                Left err -> show err

-- Just 5 >>= g --> Just 10
-- Nothing >>= g --> Nothing
g :: Int -> Maybe Int
g k = Just (2 * k)