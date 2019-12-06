#include <stdlib.h>

#include "common.h"
#include "memory.h"
#include "vm.h"

void* reallocate(void* previous, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(previous);
    return NULL;
  }

  return realloc(previous, newSize);
}

void freeObject(Obj* object) {
  switch (object->type) {
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1); // +1 for null char
      FREE(ObjString, object);
      break;
    }
  }
}

void freeObjects() {
  Obj* object = vm.objects;

  while (object != NULL) {
    // tmp reference next, free current, set current to next
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
}
