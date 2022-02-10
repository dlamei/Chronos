CC          = g++
CFLAG				= -Wall -Werror
PROG_NAME 	= Chronos

SRC_DIR 		=	./src
BIN_DIR 		= ./bin

SRC_LIST = $(wildcard $(SRC_DIR)/*.cpp)

Chronos: $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp
	mkdir -p $(BIN_DIR)
	g++ $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp $(SRC_DIR)/Parser.cpp -o $(BIN_DIR)/Chronos

asm: $(BIN_DIR)/main.o
	gcc -m32 $(BIN_DIR)/main.o -o $(BIN_DIR)/main

$(BIN_DIR)/main.o: $(SRC_DIR)/main.asm
	mkdir -p $(BIN_DIR)
	nasm -f elf32 $(SRC_DIR)/main.asm -o $(BIN_DIR)/main.o

