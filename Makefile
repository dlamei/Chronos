CFLAG				= -Wall -Werror
PROG_NAME 	= Chronos

SRC_DIR 		=	./src
BIN_DIR 		= ./bin

SRC_LIST = $(wildcard $(SRC_DIR)/*.cpp)

Chronos: $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp $(SRC_DIR)/Error.cpp $(SRC_DIR)/Compiler.cpp $(BIN_DIR)/64chlib.o
	mkdir -p $(BIN_DIR)
	g++ -std=c++17 $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp $(SRC_DIR)/Error.cpp $(SRC_DIR)/Compiler.cpp $(BIN_DIR)/64chlib.o -o $(BIN_DIR)/Chronos

asm: $(BIN_DIR)/asm.o $(BIN_DIR)/32chlib.o
	gcc -m32 $(BIN_DIR)/asm.o $(BIN_DIR)/32chlib.o -o $(BIN_DIR)/asm

$(BIN_DIR)/64chlib.o: $(SRC_DIR)/chlib.c
	gcc $(SRC_DIR)/chlib.c -c -o $(BIN_DIR)/64chlib.o

$(BIN_DIR)/32chlib.o: $(SRC_DIR)/chlib.c
	gcc -m32 $(SRC_DIR)/chlib.c -c -o $(BIN_DIR)/32chlib.o

$(BIN_DIR)/asm.o: Chronos.asm
	mkdir -p $(BIN_DIR)
	nasm -f elf32 Chronos.asm -o $(BIN_DIR)/asm.o

