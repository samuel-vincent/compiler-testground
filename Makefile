
SRC=$(wildcard *.asm)
EXEC=$(SRC:%.asm=%)
OBJ=$(SRC:%.asm=%.o)

compiler: compiler.hs
	ghc --make compiler
	./compiler "./test.asm" ""

compile: $(EXEC)

%.o: %.asm
	nasm -f elf -g -F dwarf $<

%: %.o
	gcc $< -o $@

clean:
	@rm $(EXEC) $(OBJ) $(SRC) *~ compiler.o compiler.hi compiler