
module Parser1 where
import Data.Char
import Data.List

-- Parser type

newtype Ps a = P ([Char] -> Maybe (a, [Char]))

instance Monad Ps where
  p >>= fn = P (\inp -> case parse p inp 
                        of Just (v, cs) -> parse (fn v) cs
                           Nothing -> Nothing)
  return v = P (\inp -> Just (v, inp))

-- Unwrap function from monad

parse :: Ps a -> ([Char] -> Maybe (a, [Char]))
parse (P p) inp = p inp

-- AST structure

data Type = IntT
          | CharT
          | BoolT
          | ArrayT Type
          deriving Show

data Stmts = Stmts [Stmt] deriving Show

data Stmt = Module String Stmts
          | Declare Type String 
          | Assign String Expr
          | DeclareAndAssign Type String Expr
          | If Expr [Stmt]
          | While Expr [Stmt]
          deriving Show

data Expr = Addop Expr Expr
          | Mulop Expr Expr
          | Subop Expr Expr
          | Divop Expr Expr
          | ConstNum Int  
          | Var [Char] 
          | Eql Expr Expr 
          | Neql Expr Expr 
          | Lt Expr Expr 
          | Gt Expr Expr 
          | Ge Expr Expr 
          | Le Expr Expr 
          | Or Expr Expr
          | And Expr Expr
          | TrueConst
          | FalseConst 
          deriving Show

-- Combinators

infix 9 #
infix 8 -#
infix 7 #-
infix 6 ?
infix 5 #>
infixl 4 <>

(?) :: Ps a -> (a -> Bool) -> Ps a
p ? fn = P (\inp -> case parse p inp 
                    of Just (v, cs) -> if fn v then Just (v, cs)
                                       else Nothing
                       Nothing -> Nothing)

(#) :: Ps a -> Ps b -> Ps (a, b)
p1 # p2 = P (\inp -> case parse p1 inp of 
                Just (v1, cs) -> 
                  case parse p2 cs of 
                    Just (v2, cs') -> Just ((v1, v2), cs')
                    Nothing -> Nothing
                Nothing -> Nothing)

(#>) :: Ps a -> (a -> b) -> Ps b
p #> fn = P (\inp -> case parse p inp 
                     of Just (v, cs) -> Just (fn v, cs)
                        Nothing -> Nothing)

(<>) :: Ps a -> Ps a -> Ps a 
p1 <> p2 = P (\inp -> case parse p1 inp 
                      of Just (v, cs) -> Just (v, cs)  
                         Nothing -> parse p2 inp)

(-#) :: Ps a -> Ps b -> Ps b
p1 -# p2 = P (\inp -> case parse p1 inp 
                      of Just (c, cs) -> parse p2 cs 
                         Nothing -> Nothing)

(#-) :: Ps a -> Ps b -> Ps a
p1 #- p2 = P (\inp -> case parse p1 inp 
                      of Just (c, cs) -> 
                           case parse p2 cs 
                           of Nothing -> Nothing
                              Just (c', cs') -> Just (c, cs')
                         Nothing -> Nothing)

-- Parsers

nothing :: Ps a
nothing = P (\_ -> Nothing)

char :: Ps Char
char = P (\inp -> case inp of (c:cs) -> Just (c, cs)
                              [] -> Nothing)

lit :: Char -> Ps Char
lit v = char ? (==v)

iter :: Int -> Ps a -> Ps [a]
iter n p | n > 0 = p # iter (n - 1) p #> cons
         | otherwise = return []

while :: Ps a -> Ps [a]
while p = p # while p #> cons <> return []

match :: [Char] -> Ps [Char]
match w = iter (length w) char ? (==w)

cons :: (a, [a]) -> [a]
cons (a, as) = a:as

toInt :: String -> Int
toInt v = read v :: Int 

number :: Ps Int
number = skipSpace -# while (char ? isNumber) #- skipSpace
         >>= (\inp -> case inp 
                      of [] -> nothing
                         val -> return (toInt val))

whileNot :: Char -> Ps [Char]
whileNot a = while (char ? (/=a))

alphaNum :: Ps Char
alphaNum = char ? isAlphaNum

word :: Ps [Char]
word = while alphaNum

token :: Ps [Char]
token = skipSpace -# word #- skipSpace

skipSpace :: Ps [Char]
skipSpace = while (char ? isSpace)

matchToken :: [Char] -> Ps [Char]
matchToken w = skipSpace -# match w #- skipSpace

contains :: [Char] -> [[Char]] -> Bool
contains v ws = case find (==v) ws of Nothing -> False 
                                      _ -> True

reserved = ["if", "while", "true", "false", "int", "bool", "char", "start", "end"]

var = token ? (\inp -> (inp /="") && (not (contains inp reserved))) #> Var 

num = number #> ConstNum  

term = num 
       <> bool
       <> matchToken "(" -# var #- matchToken ")"
       <> var

factor = (term #- lit '*') # factor #> (\(a, b) -> Mulop a b) 
         <> term

expr = (factor #- lit '+') # expr #> (\(a, b) -> Addop a b) 
       <> factor

true = matchToken "true" #> (\_ -> TrueConst)
false = matchToken "false" #> (\_ -> FalseConst)
bool = true <> false

lt = (expr #- matchToken "<") # rel #> (\(a, b) -> Lt a b)
gt = (expr #- matchToken ">") # rel #> (\(a, b) -> Gt a b)
ge = (expr #- matchToken ">=") # rel #> (\(a, b) -> Ge a b)
le = (expr #- matchToken "<=") # rel #> (\(a, b) -> Le a b)
rel = lt <> gt <> ge <> le <> expr

eq = (rel #- matchToken "==") # eql #> (\(a, b) -> Eql a b)
neq = (rel #- matchToken "!=") # eql #> (\(a, b) -> Neql a b)
eql = eq <> neq <> rel

or' = (eql #- matchToken "||") # or' #> (\(a, b) -> Or a b) <> eql
and' = (or' #- matchToken "&&") # and' #> (\(a, b) -> And a b) <> or'

parseType' = matchToken "int" #> (\_ -> IntT) 
             <> matchToken "char" #> (\_ -> CharT)
             <> matchToken "bool" #> (\_ -> BoolT)

parseType = parseType' #- matchToken "[]" #> ArrayT 
            <> parseType'

declare = (parseType # word #> (\(a, b) -> Declare a b)) #- matchToken ";"

assign = ((token #- matchToken "=") # expr #> (\(a, b) -> Assign a b)) #- matchToken ";"

declareAndAssign = ((parseType # (token #- matchToken "=")) 
                   # expr #> (\((a, b), c) -> DeclareAndAssign a b c)) #- matchToken ";"

startModule = matchToken "start" -# token

endModule m = matchToken "end" -# token ? (==m)

if' = ((matchToken "if" # matchToken "(") 
      -# and' # ((matchToken ")" # matchToken "{") -# stmts #- matchToken "}"))
      #> (\(a, b) -> If a b)

while' = ((matchToken "while" # matchToken "(") 
      -# and' # ((matchToken ")" # matchToken "{") -# stmts #- matchToken "}"))
      #> (\(a, b) -> While a b)

stmt = (declare <> assign <> if' <> while' <> declareAndAssign)

stmts = while stmt

parseModule = (startModule # stmts) 
              >>= (\(m, sts) -> endModule m -# return (Module m (Stmts sts)))