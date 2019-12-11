#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "debug.h"
#include "vm.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"

// create one global VM object instead of a pointer to a VM that we pass around
// this means we can't pass around a VM between applications, or initialize multiple VMs
VM vm;

// native fn for clocks
static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  // point stackTop to beginning of stack to indicate emptiness
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  // retern error callstack from most recent to least recent
  for (int i = vm.frameCount - i; i <= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;

    // -1 because IP is sitting on next instruction to execute
    // where is frame IP (aka 100) minus where is all the function code (aka 60)
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}


static void defineNative(const char* name, NativeFn function) {
  // make a copy of the name of the function and push onto VM stack so GC doesnt take it
  // push a native copy of the function
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  // set a global variable assigning name to the function;
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  // then we discard
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  initTable(&vm.strings);
  initTable(&vm.globals);

  defineNative("clock", clockNative);
}

void freeVM(){
  freeTable(&vm.strings);
  freeTable(&vm.globals);
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

static bool call(ObjClosure* closure, int argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d", closure->function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    // just in case of bad recursion
    runtimeError("Stack overflow.");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;

  /*
    This simply initializes the next CallFrame on the stack.
    It stores a pointer to the function being called and points the frame’s ip to the
    beginning of the function’s bytecode.
    Finally, it sets up the slots pointer to give the frame its window into the stack.
    The arithmetic there ensures that the arguments already on the stack line up
    with the function’s parameters:
  */

  // - 1 for the name of the function
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_CLOSURE: {
        return call(AS_CLOSURE(callee), argCount);
      }
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
      default:
        // noncallable objects (like a string)
        break;
    }
  }

  runtimeError("Can only call functions and classes.");
  return false;
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
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
  // need a local macro to get the next instruction location and increment
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
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
      // cast frame->IP to an int, then get it's relative position by subtracting where the code is
      disassembleInstruction(&frame->closure->function->chunk,
        (int)(frame->ip - frame->closure->function->chunk.code));
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
      case OP_POP: pop(); break;
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        // if it's a new key, error. No implicit declarations, we need to declare w var
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        frame->slots[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        // when we see an if statement, the next two bytes are the jump to the false branch
        // look at the top of the stack
        // if it's false, increment IP by the offset (aka skip the then branch, go to else branch)
        // otherwise, continue to then branch
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_CALL: {
        // each CallFrame has, at the top, the number of arguments
        // looking back that far in the stack shows us where the fn is
        // https://craftinginterpreters.com/image/calls-and-functions/overlapping-windows.png
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        // new frame on top, the called function
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLOSURE: {
        // emitBytes(OP_CLOSURE, second arg is location in constant pool of compiled function)
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
        break;
      }
      case OP_RETURN: {
        // when we return from a function, the result of the fn is on top
        Value result = pop();
        vm.frameCount--;

        // if there are no more frames, then we pop the entire stack AKA close the interpreter
        if (vm.frameCount == 0) {
          pop(); // the main top-level script function we injected at slot 0
          return INTERPRET_OK;
        }

        // else, throw away the frame's slots by resetting VM stackTop to start of frame's slots
        vm.stackTop = frame->slots;
        push(result);

        frame = &vm.frames[vm.frameCount - 1]; // the precending frame, the one before the function we returned from
        break;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_STRING
#undef READ_SHORT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  /*
    first, compile code and get a function
    if nothing, compilation error so throw it away
    else, store the function on the stack.
    THEN, set up the function with a new framecount, the slots from the VM stack
  */
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;

  push(OBJ_VAL(function));
  // setup first frame with closure
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  callValue(OBJ_VAL(closure), 0); // set up first frame for top level code

  return run();
}
