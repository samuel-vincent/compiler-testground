
import System(getArgs)

main :: IO ()
main = do
  (f:src:[]) <- getArgs
  writeFile f (compile src)

insn :: String -> String
insn x = "\n\t" ++ x

gen :: Expr -> String
gen (Addop l r) = insn "mov rax, " ++ l ++ 
                  insn "mov rbx, " ++ r ++ 
                  insn "add rax, rbx"

header :: String
header = "extern printf\n" ++ 
         "segment .data\n\t" ++ 
         "msg db \"Result: %i\", 0xA\n" ++
         "segment .bss\n" ++ 
         "segment .text\n\t" ++
         "global main\n" ++
         "main:\n\t" ++
         "push rbp\n\t" ++
         "mov rbp, rsp\n"
         
footer :: String
footer = "\t" ++
         "pop rsi\n\t" ++
         "mov rdi, msg\n\t" ++
         "call printf\n\t" ++
         "add rsp, 8\n\t" ++
         "mov rax, 0\n\t" ++
         "leave\n\t" ++
         "ret"

compile :: String -> String
compile "" = header ++ gen (Addop (Const 10) (Const 20)) ++ footer
compile x = header ++ gen (Addop (Const 20) (Const 30)) ++ footer

data Expr = Addop Expr Expr deriving (Show)