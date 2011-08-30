
import System(getArgs)

main :: IO ()
main = do
  (f:src:[]) <- getArgs
  writeFile f (compile src)

insn :: String -> String
insn x = "\n\t" ++ x

gen :: Expr -> String
gen (Addop l r) = gen l ++ gen r ++
                  insn "pop eax" ++
                  insn "pop ebx" ++
                  insn "add eax, ebx" ++
                  insn "push eax"

gen (Subop l r) = gen l ++ gen r ++
                  insn "pop eax" ++
                  insn "pop ebx" ++
                  insn "sub eax, ebx" ++
                  insn "neg eax" ++
                  insn "push eax"


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
compile "" = header ++ gen (Subop (Const 100) (Addop (Const 20) (Const 50))) ++ footer
compile x = header ++ gen (Addop (Const 20) (Const 30)) ++ footer

data Expr = Addop Expr Expr 
          | Subop Expr Expr 
          | Const Int 
          deriving (Show)

