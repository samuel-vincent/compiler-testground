
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

infix 3 !
(!) :: Ps a -> Ps a -> Ps a
p1 ! p2 = (\i -> case p1 i of Nothing -> p2 i
                              ret -> ret)


lit :: Char -> Ps Char
lit c = char ? (==c)

infix 6 #
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
iter p n | n > 0 = p # iter p (n - 1) >-> cons 
         | otherwise = ret []

cons :: (a, [a]) -> [a]
cons (a, as) = a:as