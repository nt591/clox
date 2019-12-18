#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLOSURE(vlaue) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE,
} ObjType;

struct sObj {
  ObjType type;
  bool isMarked;
  struct sObj* next;  // create a linked list of objects as they're created so we can later free memory when not in use
};

// struct to contain chunks for each function we create in our VM
typedef struct {
  Obj obj;
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

// our native functions to reach into C
typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

/*
  Because ObjString is an Obj, it also needs the state all Objs share.
  It accomplishes that by having its first field be an Obj.
  C specifies that struct fields are arranged in memory in the order that they are declared.
  Also, when you nest structs, the inner struct’s fields are expanded right in place.
  So the memory for Obj and for ObjString looks like this:
*/
struct sObjString {
  Obj obj;
  int length;
  char* chars;
  uint32_t hash; // storing the hash code for a given string
};

// runtime representation of our Upvalue objects
typedef struct sUpvalue {
  Obj obj;
  Value* location; // pointer to where the variable is in bytecode
  Value closed; // when we close over a variable, we need to hold onto it
  struct sUpvalue* next; // holding onto a linked list of upvalues for our VM
} ObjUpvalue;

// all functions are wrapped by closures, even if they don't capture any values
typedef struct {
  Obj obj;
  ObjFunction* function;
  // upvalues are dynamically allocated, so we need pointer
  // closure's array of upvalues is therefore also dynamic, so we need double pointer
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;

ObjClosure* newClosure(ObjFunction* function);

ObjFunction* newFunction();

// takes a C function pointer and wraps in an Object
ObjNative* newNative(NativeFn function);

ObjString* takeString(char* chars, int length);

ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);

void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif