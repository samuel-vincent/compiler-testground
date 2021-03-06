
SRC=$(wildcard *.asm)
EXEC=$(SRC:%.asm=%)
OBJ=$(SRC:%.asm=%.o)

LBITS=$(shell getconf LONG_BIT)

ifeq ($(LBITS),64)
	ASM_FMT=elf64
	ARCH="x86-64"
else
	ASM_FMT=elf
	ARCH="x86-32"
endif

Compiler1: Compiler1.hs
	ghc --make Compiler1
	./Compiler1 "./test.asm" "start Test1 int a = 100; a = a + 300; end Test1" $(ARCH)

compile: $(EXEC)

%.o: %.asm
	nasm -f $(ASM_FMT) -g -F dwarf -l $@.lst $<

%: %.o
	gcc -gdwarf-2 $< -o $@

test: x64.nasm
	nasm -f elf64 x64.nasm
	gcc x64.o -o x64
	./x64

.PHONY: clean
clean:
	@rm $(EXEC) $(SRC) *.hi *.o *.lst Compiler1
