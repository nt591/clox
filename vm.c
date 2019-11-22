#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "debug.h"
#include "vm.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"

// create one global VM object instead of a pointer to a VM that we pass around
// this means we can't pass around a VM between applications, or initialize multiple VMs
VM vm;

static void resetStack() {
  // point stackTop to beginning of stack to indicate emptiness
  vm.stackTop = vm.stack;
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  size_t instruction = vm.ip - vm.chunk->code;
  int line = vm.chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);

  resetStack();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  initTable(&vm.strings);
}

void freeVM(){
  freeTable(&vm.strings);
  freeObjects();
}

void push(Value value) {
  /*
    The first line stores value in the array element at the top of the stack.
    Remember, stackTop points just past the last used element, at the next available one.
    This stores the value in that slot.
  */
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

static bool isFalsey(Value value) {
  // a value is false if either it's nil OR
  // if it's a bool, return the opposite value
  // aka, false returns true for falsey, true returns false for falsiness
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1); // + 1 for null char
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  push(OBJ_VAL(result));
}

static InterpretResult run() {
  // need a local macro to get the next instruction location and increment
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)

  for (;;) {
    #ifndef DEBUG_TRACE_EXECUTION
      printf("          ");
      for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        printf("[ ");
        printValue(*slot);
        printf(" ]");
      }
      printf("\n");
      // cast vm.IP to an int, then get it's relative position by subtracting where the code is
      disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
    #endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_NIL: push(NIL_VAL); break;
      case OP_TRUE: push(BOOL_VAL(true)); break;
      case OP_FALSE: push(BOOL_VAL(false)); break;
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      // pop the last seen value, negate it, push it back onto the stack
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          // top value on stack isn't a number, cannot negate
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }

        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      case OP_GREATER: BINARY_OP(BOOL_VAL, >); break; // get the last two vals on the stack if they're numbers, and return a > b as a bool
      case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError("Operands must be two numbers or two string.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
      case OP_PRINT:
        printValue(pop());
        printf("\n");
        break;
      case OP_RETURN: {
        // exit
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  // create a chunk of data
  // if you can compile source, fill the chunk with bytecode
  // else error
  // once you have a chunk, give it to the VM to execute
  Chunk chunk;
  initChunk(&chunk);

  if (!compile(source, &chunk)) {
    freeChunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code;

  InterpretResult result = run();

  freeChunk(&chunk);
  return result;
}
