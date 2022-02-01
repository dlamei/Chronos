#CC          = g++
#LD          = g++ 
#CFLAG       = -Wall
#PROG_NAME   = Chronos
#
#SRC_DIR     = ./src
#BUILD_DIR   = ./build
#BIN_DIR     = ./bin
#SRC_LIST = $(wildcard $(SRC_DIR)/*.cpp)
#OBJ_LIST = $(BUILD_DIR)/$(notdir $(SRC_LIST:.cpp=.o))
#
#.PHONY: all clean $(PROG_NAME) compile
#
#all: $(PROG_NAME)
#
#compile: 
#	mkdir -p $(BIN_DIR)
#	$(CC) -c $(CFLAG) $(SRC_LIST) -o $(OBJ_LIST)
#
#$(PROG_NAME): compile
#	$(LD) $(OBJ_LIST) -o $(BIN_DIR)/$@
#
#clean:
#	rm -f $(BIN_DIR)/$(PROG_NAME) $(BUILD_DIR)/*.o

CC          = g++
CFLAG				= -Wall
PROG_NAME 	= Chronos

SRC_DIR 		=	./src
BIN_DIR 		= ./bin

SRC_LIST = $(wildcard $(SRC_DIR)/*.cpp)

Chronos: $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp
	mkdir -p $(BIN_DIR)
	g++ $(SRC_DIR)/main.cpp $(SRC_DIR)/lexer.cpp -o $(BIN_DIR)/Chronos

