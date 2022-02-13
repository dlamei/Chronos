CC          = g++
CFLAG				= -Wall -Werror
PROG_NAME 	= Chronos

SRC_DIR 		=	./src
BIN_DIR 		= ./bin

SRC_LIST = $(wildcard $(SRC_DIR)/*.cpp)

Chronos: $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp $(SRC_DIR)/Compiler.cpp
	mkdir -p $(BIN_DIR)
	g++ $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp $(SRC_DIR)/Compiler.cpp -o $(BIN_DIR)/Chronos

asm: $(BIN_DIR)/asm.o
	gcc -m32 $(BIN_DIR)/asm.o -o $(BIN_DIR)/asm

$(BIN_DIR)/asm.o: Chronos.asm
	mkdir -p $(BIN_DIR)
	nasm -f elf32 Chronos.asm -o $(BIN_DIR)/asm.o

