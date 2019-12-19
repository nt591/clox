#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
  (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
  // the caller knows the size of the underlying type, so it passes that in
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  object->isMarked = false;
  // vm holds onto the head of the linked list, so every new object becomes the head,
  // and we point the new head to the old head
  object->next = vm.objects;
  vm.objects = object;


#ifdef DEBUG_LOG_GC
  printf("%p allocate %ld for %d\n", (void*)object, size, type);
#endif

  return object;
}

ObjClosure* newClosure(ObjFunction* function) {
  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

  function->arity = 0;
  function->upvalueCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjNative* newNative(NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
}

static ObjString* allocateString(char* chars, int length, uint32_t hash) {
  // allocate space in memory for a string object, then set its values
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;

  // adding to the table can trigger reallocation
  // reallocation triggers GC
  // if it GC's, string isn't reachable from a root so it'll get swept so we need to protect in the stack
  push(OBJ_VAL(string));
  tableSet(&vm.strings, string, NIL_VAL); // store in a hash table every string we see for the VM
  pop();
  return string;
}

static uint32_t hashString(const char* key, int length) {
  // FNV-1a hashing
  uint32_t hash = 2166136261u;

  for (int i = 0; i < length; i++) {
    // XOR the hash with the char
    // then multiple by some magic num
    hash ^= key[i];
    hash *= 16777619;
  }

  return hash;
}

ObjString* takeString(char* chars, int length) {
  uint32_t hash = hashString(chars, length);

  // Again, we look up the string in the string table first.
  //  If we find it, before we return it, we free the memory for the string that was passed in.
  // Since ownership is being passed to this function and we no longer need the duplicate string, it’s up to us to free it.
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }
  return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
  uint32_t hash = hashString(chars, length);

  // When copying a string into a new LoxString, we look it up in the string table first.
  // If we find it, instead of “copying”, we just return a reference to that string.
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) return interned;

  char* heapChars = ALLOCATE(char, length + 1); // add one for null char to terminate string
  // heapChars is a pointer to a address big enough for all the chars + a terminator
  memcpy(heapChars, chars, length);
  // copy to heapChars all my chars, and make it length long
  // terminate the end
  heapChars[length] = '\0';

  return allocateString(heapChars, length, hash);
}

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->closed = NIL_VAL;
  upvalue->location = slot;
  upvalue->next = NULL;
  return upvalue;
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
  switch(OBJ_TYPE(value)) {
    case OBJ_CLOSURE:
      printFunction(AS_CLOSURE(value)->function);
    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
    case OBJ_UPVALUE:
      printf("upvalue");
      break;
  }
}