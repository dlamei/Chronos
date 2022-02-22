#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
//#include <cassert>

#include "chlib.h"

typedef enum mark_type MarkType;
typedef struct block_header BlockHeader;
typedef struct heap_block HeapBlock;


HeapBlock* alloc_heap_block()
{
	HeapBlock* block = malloc(sizeof(HeapBlock));
	if (!block) exit(-1);

	if (!block->memory)
	{
		printf("could not allocate memory");
		exit(-1);
	}

	block->header.block_mark = FREE;
	for (int i = 0; i < LINE_COUNT; i++)
	{
		block->header.line_mark[i] = FREE;
	}

	return block;
}

void free_heap_block(HeapBlock* block)
{
	free(block);
}

bool find_next_hole(BlockHeader* header, uint32_t start_indx, int* cursor, int* limit)
{
	uint32_t start_line = start_indx / LINE_SIZE;
	uint32_t count = 0, begin = 0, end = 0;

	bool found = false;

	for (int i = 0; i < LINE_COUNT; ++i)
	{
		uint32_t line_indx = start_line + i;
		end = line_indx + 1;

		if (header->line_mark[line_indx] == FREE && !found)
		{
			begin = line_indx;
		}
		
		if (header->line_mark[line_indx] == FREE)
		{
			found = true;
			count++;
		}
		else if (found) break;
	}

	if (!found) return false;

	*cursor = begin * LINE_SIZE;
	*limit = end * LINE_SIZE;
	return true;
}

typedef struct bump_block BumpBlock;

BumpBlock* alloc_bump_block()
{
	BumpBlock* bump_block = malloc(sizeof(BumpBlock));

	if (!bump_block)
	{
		printf("could not allocate memory");
		exit(-1);
	}

	bump_block->cursor = 0;
	bump_block->limit = BLOCK_SIZE;
	//bump_block->data = alloc_heap_block();

	bump_block->data.header.block_mark = FREE;
	for (int i = 0; i < LINE_COUNT; i++)
	{
		bump_block->data.header.line_mark[i] = FREE;
	}

	return bump_block;
}

void free_bump_block(BumpBlock* block)
{
	//free_heap_block(block->data);
}

byte* bump_reserve_size(BumpBlock* block, uint32_t alloc_size)
{
	block->data.header.block_mark = MARKED;

	uint32_t next_bump = block->cursor + alloc_size;

	if (next_bump <= block->limit)
	{
		block->data.header.line_mark[block->cursor / LINE_SIZE] = MARKED;
		for (uint32_t i = 1; i < alloc_size / LINE_SIZE; i++)
		{
			block->data.header.line_mark[block->cursor / LINE_SIZE + i] = CONS_MARKED;
		}

		byte* ptr = &block->data.memory[block->cursor];
		block->cursor = next_bump;
		return ptr;
	} 
	else if (block->limit < BLOCK_SIZE)
	{
		if (find_next_hole(&block->data.header, block->limit, block->cursor, block->limit))
		{
			return bump_reserve_size(block, alloc_size);
		}
		else
		{
			return NULL;
		}
	}
	return NULL;
}

typedef enum ch_type ChType;
typedef struct ch_header ChHeader;
typedef struct ch_int ChInt;

int type_from_ptr(BumpBlock* block, void* ptr)
{
	ChHeader* header = ptr;
	return header->type;
}

void print_line_marks(BumpBlock* block)
{
	printf("{ [");
	if (block->data.header.line_mark == FREE) printf("_");
	else if (block->data.header.line_mark != FREE) printf("M");
	printf("]: ");
	printf("[");
	for (int i = 0; i < LINE_COUNT; ++i)
	{
		if (block->data.header.line_mark[i] == FREE) printf("_");
		else if (block->data.header.line_mark[i] == CONS_MARKED) printf("C");
		else if (block->data.header.line_mark[i] == MARKED) printf("M");
		else printf("e");
	}
	printf("] }\n");
}

ChInt* bump_write_int(BumpBlock* block, int value)
{
	ChInt* ptr = bump_reserve_size(block, sizeof(ChInt));
	if (!ptr) return NULL;
	ptr->header.type = CH_INT;
	ptr->value = value;
	return ptr;
}

typedef struct ch_heap ChHeap;

ChHeap* alloc_heap()
{
	ChHeap* h = malloc(sizeof(ChHeap));
	h->blocks = alloc_bump_block();
	return h;
}

struct ch_int* heap_alloc_int(ChHeap* heap, int value)
{
	//TODO: multiple blocks + resize
	return bump_write_int(heap->blocks, value);
}
