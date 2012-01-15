
module Main where
import Data.Char

data Expr = Num Int 
          | Var String 
          | Add Expr Expr 
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr 
            deriving (Show)

data Stmt = Assign String Expr 
            deriving (Show)

type Ps a = String -> Maybe (a, String)

parse :: Ps a -> String -> Maybe (a, String)
parse p s = p s

char :: Ps Char
char = (\i -> case i of (c:cs) -> Just (c, cs)
                        [] -> Nothing)

nil :: Ps t
nil _ = Nothing

ret :: t -> Ps t
ret t = (\i -> Just (t, i))

infix 7 ?
(?) :: Ps a -> (a -> Bool) -> Ps a
p ? fn = (\i -> case p i of 
                  Nothing -> Nothing
                  Just (x, xs) -> if (fn x) then Just (x, xs) else Nothing)

digit :: Ps Char
digit = char ? isDigit

infixl 3 !
(!) :: Ps a -> Ps a -> Ps a
p1 ! p2 = (\i -> case p1 i of Nothing -> p2 i
                              ret -> ret)
lit :: Char -> Ps Char
lit c = char ? (==c)

infixl 6 #
(#) :: Ps a -> Ps b -> Ps (a, b)
p1 # p2 = (\i -> case p1 i of Nothing -> Nothing
                              Just (t1, cs1) -> 
                                  case p2 cs1 of 
                                    Nothing -> Nothing
                                    Just (t2, cs2) -> Just ((t1, t2), cs2))

semicolon :: Ps Char
semicolon = lit ';' 

becomes :: Ps (Char, Char)
becomes = lit ':' # lit '='

infixl 5 >->
(>->) :: Ps a -> (a -> b) -> Ps b
p >-> fn = (\i -> case p i of Nothing -> Nothing
                              Just (t, cs) -> Just (fn t, cs))

digitVal :: Ps Int
digitVal = digit >-> digitToInt

upper :: Ps Char
upper = char >-> toUpper

sndchar :: Ps Char
sndchar = (\i -> case (char # char) i of 
                   Just ((a, b), cs) -> Just (b, cs)
                   _ -> Nothing)

(-#) :: Ps a -> Ps b -> Ps b
p1 -# p2 = (\i -> case p1 i of Nothing -> Nothing
                               Just (t1, cs) -> p2 cs)

(#-) :: Ps a -> Ps b -> Ps a
p1 #- p2 = (\i -> case p1 i of 
                    Nothing -> Nothing
                    Just (t1, cs1) -> 
                        case p2 cs1 of 
                          Nothing -> Nothing
                          Just (t2, cs2) -> Just (t1, cs2))

iter :: Ps a -> Int -> Ps [a]
iter p n | n > 0 = (p # iter p (n - 1)) >-> cons
         | otherwise = ret []

iterWhile :: Ps a -> Ps [a]
iterWhile p = p # iterWhile p >-> cons ! ret []

cons :: (a, [a]) -> [a]
cons (a, as) = a:as

letter :: Ps Char
letter = char ? isAlpha

letters :: Ps String
letters = iterWhile letter

token :: Ps a -> Ps a
token p = p #- (iterWhile (char ? isSpace))

word :: Ps String
word = token letters ? (/="")

accept :: String -> Ps String
accept w = token (iter char (length w) ? (==w)) 

infix 4 #>
(#>) :: Ps a -> (a -> Ps b) -> Ps b
p #> fn = (\s -> case p s of Nothing -> Nothing
                             Just (a, cs) -> fn a cs)

double :: Ps Char
double = char #> lit

bldNum :: Int -> Int -> Int
bldNum n d = 10 * n + d

number' :: Int -> Ps Int
number' n = digitVal >-> bldNum n #> number' ! ret n

number :: Ps Int
number = token (digitVal #> number')

mulOp :: Ps (Expr -> Expr -> Expr)
mulOp = lit '*' >-> (\_ -> Mul) 
        ! lit '/' >-> (\_ -> Div)

addOp :: Ps (Expr -> Expr -> Expr)
addOp = lit '+' >-> (\_ -> Add) 
        ! lit '-' >-> (\_ -> Sub)

var :: Ps Expr
var = word >-> Var

num :: Ps Expr
num = number >-> Num


factor :: Ps Expr
factor = num ! var ! lit '(' -# var #- lit ')'

term' :: Expr -> Ps Expr
term' e = (((mulOp # factor) >-> bldOp e) #> term') ! ret e

bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (oper, e') = oper e e'

term :: Ps Expr
term = factor #> term'

expr' e = (((addOp # term) >-> bldOp e) #> expr') ! ret e
expr = term #> expr'