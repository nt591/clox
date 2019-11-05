#include <stdio.h>
#include "common.h"
#include "debug.h"
#include "vm.h"

// create one global VM object instead of a pointer to a VM that we pass around
// this means we can't pass around a VM between applications, or initialize multiple VMs
VM vm;

void initVM() {

}

void freeVM(){

}

static InterpretResult run() {
  // need a local macro to get the next instruction location and increment
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
  for (;;) {
    #ifndef DEBUG_TRACE_EXECUTION
      // cast vm.IP to an int, then get it's relative position by subtracting where the code is
      disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
    #endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        printValue(constant);
        printf("\n");
        break;
      }
      case OP_RETURN: {
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
  vm.chunk = chunk;
  // the instruction pointer is the code from the chunk we added
  vm.ip = vm.chunk->code;
  return run();
}
