
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
	./compiler "./test.asm" "" $(ARCH)

compile: $(EXEC)

%.o: %.asm
	nasm -f $(ASM_FMT) -g -F dwarf $<

%: %.o
	gcc $< -o $@

clean:
	@rm $(EXEC) $(OBJ) $(SRC) *~ compiler.o compiler.hi compiler