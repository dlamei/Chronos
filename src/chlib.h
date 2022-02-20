#pragma once


typedef char byte;

struct heap_block;

struct heap_block* heap_alloc_block();
void free_heap_block(struct heap_block* block);
byte* reserve_block_memory(struct heap_block* block, uint32_t byte_size);