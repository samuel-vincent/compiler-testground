module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr s = case parse parseExpr "scheme" s of
               Left err -> String $ "No match: " ++ show err
               Right v -> v

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = char '"' >>= \_ ->
              many (noneOf "\"") >>= \v ->
              char '"' >>= \_ -> return $ String v

parseAtom :: Parser LispVal
parseAtom = (letter <|> symbol) >>= \fst ->
            many (letter <|> digit <|> symbol) >>= \rest ->
            return $ case (fst:rest) of
                       "#t" -> Bool True
                       "#f" -> Bool False
                       _ -> Atom (fst:rest)

-- liftM :: Monad m => (a -> b) -> m a -> m b

parseNum :: Parser LispVal
parseNum = liftM (Number . read) $ many1 digit 

parseExpr :: Parser LispVal
parseExpr = parseAtom 
            <|> parseString 
            <|> parseNum
            <|> parseQuoted
            <|> do char '('
                   v <- try parseList <|> parseDottedList
                   char ')'
                   return v

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do 
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do 
  char '\''
  v <- parseExpr
  return $ List [Atom "quote", v]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal

eval :: LispVal -> LispVal
eval v@(String _) = v
eval v@(Number _) = v
eval v@(Bool _) = v
eval (List [Atom "quote", v]) = v
eval (List (Atom fn : args)) = apply fn $ map eval args 

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                       if null parsed 
                       then 0
                       else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0