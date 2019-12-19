#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  initValueArray(&chunk->constants);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
  // if count + 1 exceeds current capacity
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code = GROW_ARRAY(chunk->code, uint8_t, oldCapacity, chunk->capacity);
    chunk->lines = GROW_ARRAY(chunk->lines, int, oldCapacity, chunk->capacity);
  }

  // code is an array, set the index to the byte
  chunk->code[chunk->count] = byte;
  // storing the line number in compiled code of every chunk
  chunk->lines[chunk->count] = line;
  chunk->count++;
}

int addConstant(Chunk* chunk, Value value) {
  // it's possible that when we write the array, we might need to grow the array and reallocate
  // which triggers a GC. In order to make sure the value doesn't get GC's, we push it to the stack first
  push(value);
  writeValueArray(&chunk->constants, value);
  pop();
  // return index of last added constant
  return chunk->constants.count - 1;
}

void freeChunk(Chunk* chunk) {
  // deallocate all the memory for the chunk, then zero out its values
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}