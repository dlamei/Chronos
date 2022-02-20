#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <cassert>

#include "chlib.h"

#define BLOCK_SIZE 32
#define LINE_SIZE 8
#define LINE_COUNT (BLOCK_SIZE / LINE_SIZE)


typedef struct heap_header
{
	bool line_mark[LINE_COUNT];
	bool block_mark;
} HeapHeader;

typedef struct heap_block
{
	HeapHeader header;
	byte* memory;

	byte* cursor;

} HeapBlock;

HeapBlock* heap_alloc_block()
{
	HeapBlock* block = malloc(sizeof(HeapBlock));
	if (!block) exit(-1);

	block->memory = malloc(BLOCK_SIZE);

	if (!block->memory)
	{
		printf("could not allocate memory");
		exit(-1);
	}

	block->cursor = block->memory;

	block->header.block_mark = false;
	//block->header.line_mark = { 0 };
	memset(block->header.line_mark, 0, LINE_COUNT);

	return block;
}

void free_heap_block(HeapBlock* block)
{
	free(block->memory);
	free(block);
}

byte* reserve_block_memory(HeapBlock* block, uint32_t byte_size)
{
	uint32_t used_space = block->cursor - block->memory;
	if (used_space >= BLOCK_SIZE) return NULL;


	uint32_t available_space = BLOCK_SIZE - used_space;

	if (available_space >= byte_size)
	{
		uint32_t current_line = used_space / LINE_COUNT;
		block->header.line_mark[current_line] = true;
		if (used_space) block->header.block_mark = true;

		byte* ptr = block->cursor;
		block->cursor += byte_size;
		return ptr;
	}
	else
	{
		return NULL;
	}
}

HeapBlock* heap_alloc_block_array()
{
	//TODO
	// for storing objects greater than 1 block
	exit(-1);
}

typedef struct heap
{
	HeapBlock block_arry[3];
} Heap;


typedef enum obj_type
{
	INT,
} ObjType;

typedef struct obj_header
{
	bool mark;
	ObjType type;
} ObjHeader;

typedef struct ch_int
{
	ObjHeader header;
	int value;
} ChInt;
