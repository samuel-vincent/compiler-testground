
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

compiler: compiler.hs
	ghc --make compiler
	./compiler "./test.asm" "start Test1 int a = 100; a = a + 300; a = 200; end Test1" $(ARCH)

compile: $(EXEC)

%.o: %.asm
	nasm -f $(ASM_FMT) -g -F dwarf $<

%: %.o
	gcc -gdwarf-2 $< -o $@

parser_types: parser_types.cpp
	g++ $< -o $@

clean:
	@rm $(EXEC) $(OBJ) $(SRC) *~ compiler.o compiler.hi compiler
