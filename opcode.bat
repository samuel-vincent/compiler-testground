
@echo off

echo [BITS 64] > opcode.nasm
echo %* >> opcode.nasm

nasm -f bin opcode.nasm -l opcode.lst -o opcode.bin

type opcode.lst

del opcode.*