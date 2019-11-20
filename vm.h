#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

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
  // create an array of values that we'll call The Stack and initialize its size to some max
  Value stack[STACK_MAX];
  /// stack top starts at pointing to element 0, and ends at element just outside stack length
  Value* stackTop;
  Table strings; // we're gonna intern our strings so when we compare, we can compare in memory
  Obj* objects; // point to head of list of objects for GC
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
void push(Value value);
Value pop();

InterpretResult interpret(const char* source);

#endif