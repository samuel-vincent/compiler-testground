
import System(getArgs)

main :: IO ()
main = do
  (f:src:[]) <- getArgs
  writeFile f (compile src)

insn :: String -> String
insn x = "\n\t" ++ x

binop :: Expr -> Expr -> String -> String
binop l r cmd = gen l ++ gen r ++
                insn "pop eax" ++
                insn "pop ebx" ++
                insn cmd ++
                insn "push eax"

gen :: Expr -> String
gen (Addop l r) = binop l r "add eax, ebx"
gen (Subop l r) = binop l r ("sub eax, ebx" ++ insn "neg eax")
gen (Mulop l r) = binop l r "mul ebx"
gen (Const x) = insn "mov eax, " ++ (show x) ++
                insn "push eax"

header :: String
header = "extern printf\n" ++ 
         "segment .data\n\t" ++ 
         "msg db \"Result: %i\", 0xA\n" ++
         "segment .bss\n" ++ 
         "segment .text\n\t" ++
         "global main\n" ++
         "main:\n\t" ++
         "push ebp\n\t" ++
         "mov ebp, esp\n"
         
footer :: String
footer = "\n\t" ++
         "push dword msg\n\t" ++
         "call printf\n\t" ++
         "add esp, 8\n\t" ++
         "mov esp, ebp\n\t" ++
         "pop ebp\n\t" ++
         "mov eax, 0\n\t" ++
         "ret"

compile :: String -> String
compile _ = header ++ 
            gen (Subop (Const 100) 
                 (Addop (Mulop (Const 20) (Const 2)) (Const 50))) ++ 
            footer

type Term = Int

data Expr = Addop Expr Expr 
          | Subop Expr Expr 
          | Mulop Expr Expr
          | Const Term
          deriving (Show)

