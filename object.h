#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
  OBJ_STRING,
} ObjType;

struct sObj {
  ObjType type;
  struct sObj* next;  // create a linked list of objects as they're created so we can later free memory when not in use
};

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
};

ObjString* takeString(char* chars, int length);

ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif