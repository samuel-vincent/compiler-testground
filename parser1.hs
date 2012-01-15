
module Parser1 where
import Data.Char

-- Parser type
newtype Ps a = P ([Char] -> Maybe (a, [Char]))

-- Evaluate parser
parse :: Ps a -> ([Char] -> Maybe (a, [Char]))
parse (P p) inp = p inp

-- Parse single char
char :: Ps Char
char = P (\inp -> case inp of (c:cs) -> Just (c, cs)
                              [] -> Nothing)

-- Test parsing result
(?) :: Ps a -> (a -> Bool) -> Ps a
p ? fn = P (\inp -> case parse p inp of Just (v, cs) -> if fn v then Just (v, cs)
                                                        else Nothing
                                        _ -> Nothing)

unit :: a -> Ps a
unit a = P (\inp -> Just (a, inp))

-- Match char
lit :: Char -> Ps Char
lit v = char ? (==v)

-- Join parsing results
(#) :: Ps a -> Ps b -> Ps (a, b)
p1 # p2 = P (\inp -> case parse p1 inp of 
                Just (v1, cs) -> 
                  case parse p2 cs of 
                    Just (v2, cs') -> Just ((v1, v2), cs')
                    _ -> Nothing
                _ -> Nothing)

-- Change type of result
(#>) :: Ps a -> (a -> b) -> Ps b
p #> fn = P (\inp -> case parse p inp of Just (v, cs) -> Just (fn v, cs)
                                         _ -> Nothing)

-- Or
(<>) :: Ps a -> Ps a -> Ps a 
p1 <> p2 = P (\inp -> case parse p1 inp of Just (v, cs) -> Just (v, cs)  
                                           _ -> parse p2 inp)

-- Iterate parser n times and join results to list
iter :: Int -> Ps a -> Ps [a]
iter n p | n > 0 = (p # iter (n - 1) p) #> cons
         | otherwise = unit []

-- Join results to list while parser succeeds
while :: Ps a -> Ps [a]
while p = ((p # while p) #> cons) <> unit []

-- Match number
num :: Ps Char
num = char ? isNumber

-- Match word
match :: [Char] -> Ps [Char]
match w = (iter (length w) char) ? (==w)

-- Construct list 
cons :: (a, [a]) -> [a]
cons (a, as) = a:as

toInt :: String -> Int
toInt v = read v :: Int 

-- Parse number
number :: Ps Int
number = (while num) #> toInt

-- Not parsers
whileNot :: Char -> Ps [Char]
whileNot a = while (char ? (/=a))

-- Space
notSpace :: Ps Char
notSpace = char ? (\inp -> not $ isSpace inp)

-- Match word until whitespace
word :: Ps [Char]
word = while notSpace

-- Pass whitespace
passSpace :: Ps [Char]
passSpace = while (char ? isSpace)

-- Pass first parser and accept second
(-#) :: Ps a -> Ps b -> Ps b
p1 -# p2 = P (\inp -> case parse p1 inp of Just (c, cs) -> parse p2 cs 
                                           _ -> Nothing)

-- Opposite
(#-) :: Ps a -> Ps b -> Ps a
p1 #- p2 = P (\inp -> case parse p1 inp 
                      of Just (c, cs) -> 
                           case parse p2 cs 
                           of Nothing -> Nothing
                              Just (c', cs') -> Just (c, cs')
                         _ -> Nothing)

data Stmt = Module String 
          | Declare String
          | Assign String
          deriving Show

data Expr = Addop Expr Expr
          | Mulop Expr Expr
          | ConstNum Int  
          | Var [Char] 
          deriving Show

var' = word #> Var

num' = number #> ConstNum  

term' = num' <> ((lit '(' -# var') #- lit ')')

mulop' (a, b) = Mulop a b

addop' (a, b) = Addop a b

factor' = (((term' #- lit '*') # factor') #> mulop') <> term'

expr' = (((factor' #- lit '+') # expr') #> addop') <> factor'

-- **************

main = (((match "module") # passSpace) -# word) #> Module

expr = expr' #- (lit ';')