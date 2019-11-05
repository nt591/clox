#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"

typedef struct {
  Chunk* chunk;
  /*
    IP = instruction pointer, or where the instruction we're about to execute is located in memory.
    Its type is a byte pointer. We use an actual real C pointer right into the middle of the bytecode array
    instead of something like an integer index because it’s faster to dereference a pointer
    than look up an element in an array by index.
    source: https://craftinginterpreters.com/a-virtual-machine.html
  */
  uint8_t* ip;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(Chunk* chunk);

#endif