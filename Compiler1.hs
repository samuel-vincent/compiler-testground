import System (getArgs)
import Parser1

main :: IO ()
main = do
  (f:src:platform:[]) <- getArgs
  writeFile f (compile src (if platform == "x86-32" then X86_32 else X86_64))

data Architecture = X86_32 | X86_64

ax :: Architecture -> String
ax (X86_32) = "eax"
ax (X86_64) = "rax"

bx :: Architecture -> String
bx (X86_32) = "ebx"
bx (X86_64) = "rbx"

insn :: String -> String
insn x = "\n\t" ++ x

binop :: Architecture -> Expr -> Expr -> String -> String
binop a l r cmd = gen a l ++ gen a r ++
                insn ("pop " ++ (ax a)) ++
                insn ("pop " ++ (bx a)) ++
                insn cmd ++
                insn ("push " ++ (ax a))

join :: [String] -> String
join (s:ss) = s ++ join ss
join [] = ""

gen :: Architecture -> Expr -> String
gen a (Addop l r) = binop a l r ("add " ++ (ax a) ++ ", " ++ (bx a))
gen a (Subop l r) = binop a l r ("sub " ++ (ax a) ++ ", " ++ (bx a) ++ (insn "neg ") ++ (ax a))
gen a (Mulop l r) = binop a l r ("mul " ++ (bx a))
gen a (ConstNum con) = insn ("mov " ++ (ax a) ++ ", " ++ (show con)) ++ 
                    insn ("push " ++ (ax a))
gen a (Var n) = insn ("mov " ++ (ax a) ++ ", " ++ "[" ++ n ++ "]") ++ 
                     insn ("push " ++ (ax a))
gen _ _ = ""

gen'' a (Module _ stmts) = gs' a stmts
gen'' a (Assign n expr) = gen a expr
gen'' _ _ = ""

gen' :: Architecture -> Stmt -> String
gen' a (Module _ stmts) = gs a stmts
gen' a (DeclareAndAssign _ n v) = insn (n ++ " dd\t" ++ (showVal v))
gen' _ _ = ""

showVal (ConstNum n) = show n
showVal _ = ""

gs a (Stmts stmts) = join [(gen' a stmt) | stmt <- stmts]
gs' a (Stmts stmts) = join [(gen'' a stmt) | stmt <- stmts]

header :: Architecture -> String -> String
header (X86_32) a = "extern printf\n" ++ 
                    "segment .data\n\t" ++
                    a ++ "\n\t" ++
                    "msg db \"Result: %i\", 0xA\n" ++
                    "segment .bss\n" ++ 
                    "segment .text\n\t" ++
                    "global main\n" ++
                    "main:\n\t" ++
                    "push ebp\n\t" ++
                    "mov ebp, esp\n"
                  
header (X86_64) a = "extern printf\n" ++
                    "segment .data\n\t" ++
                    a ++ "\n\t" ++
                    "msg db \"Result: %i\", 0xA\n" ++
                    "segment .bss\n" ++
                    "segment .text\n\t" ++
                    "global main\n" ++
                    "main:\n\t" ++
                    "push rbp\n\t" ++
                    "mov rbp, rsp\n"

footer :: Architecture -> String
footer (X86_32) = "\n\t" ++
                  "push dword msg\n\t" ++
                  "call printf\n\t" ++
                  "add esp, 8\n\t" ++
                  "mov esp, ebp\n\t" ++
                  "pop ebp\n\t" ++
                  "mov eax, 0\n\t" ++
                  "ret"
                  
footer (X86_64) = "\n\t" ++
                  "pop rsi\n\t" ++
                  "mov rdi, msg\n\t" ++
                  "call printf\n\t" ++
                  "add rsp, 8\n\t" ++
                  "mov rax, 0\n\t" ++
                  "leave\n\t" ++
                  "ret"

compile :: String -> Architecture -> String
compile s a = let ast = parse parseModule s
              in case ast of Nothing -> ""
                             Just (as, cs) -> (header a (gen' a as)) ++ (gen'' a as) ++ footer a