
SRC=$(wildcard *.asm)
EXEC=$(SRC:%.asm=%)
OBJ=$(SRC:%.asm=%.o)
LBITS=$(shell getconf LONG_BIT)

ifeq ($(LBITS),64)
	ASM_CMD=elf64
else
	ASM_CMD=elf
endif

compiler: compiler.hs
	ghc --make compiler
	./compiler "./test.asm" ""

compile: $(EXEC)

%.o: %.asm
	nasm -f $(ASM_CMD) -g -F dwarf $<

%: %.o
	gcc $< -o $@

clean:
	@rm $(EXEC) $(OBJ) $(SRC) *~ compiler.o compiler.hi compiler