#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "vm.h"
#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

void* reallocate(void* previous, size_t oldSize, size_t newSize) {
  if (newSize > oldSize) {
    #ifdef DEBUG_STRESS_GC
      collectGarbage();
    #endif
  }
  if (newSize == 0) {
    free(previous);
    return NULL;
  }

  return realloc(previous, newSize);
}

void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif

  switch (object->type) {
    case OBJ_CLOSURE: {
      // free all the upvalue pointers
      ObjClosure* closure = (ObjClosure*)object;
      FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);

      // freeing only the closure, not the function it captures
      // multiple closures can capture a function, so lets let GC handle that
      FREE(ObjClosure, object);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }
    case OBJ_NATIVE: {
      FREE(OBJ_NATIVE, object);
      break;
    }
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1); // +1 for null char
      FREE(ObjString, object);
      break;
    }
    case OBJ_UPVALUE: {
      FREE(ObjUpvalue, object);
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

  free(vm.grayStack);
}

void markObject(Obj* object) {
  if (object == NULL) return;
  if (object->isMarked) return;
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  object->isMarked = true;

  // handle gray worklist
  if (vm.grayCapacity < vm.grayCount + 1) {
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    vm.grayStack = realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
  }

  vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
  if (!IS_OBJ(value)) return;

  markObject(AS_OBJ(value));
}

static void markRoots() {
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }

  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }

  for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
    markObject((Obj*)upvalue);
  }

  markTable(&vm.globals);
  markCompilerRoots();
}

static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  // A black object is any object whose isMarked field is set and that is no longer in the gray stack.
  switch (object->type) {
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      markObject((Obj*)closure->function);
      for (int i = 0; i < closure->upvalueCount; i++) {
        markObject(closure->upvalues[i]);
      }
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      markObject((Obj*)function->name);
      markArray(&function->chunk.constants);
      break;
    }
    case OBJ_UPVALUE:
      markValue(((ObjUpvalue*)object)->closed);
      break;
    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
  }
}

static void traceReferences() {
   while (vm.grayCount > 0) {
     Obj* object = vm.grayStack[--vm.grayCount];
     blackenObject(object);
   }
}

static void sweep() {
  Obj* previous = NULL;
  Obj* object = vm.objects;

  while (object != NULL) {
    // if it's marked, it's reachable. If it's reachable, iterate
    if (object->isMarked) {
      // set it back to white for the next GC
      object->isMarked = false;

      previous = object;
      object = object->next;
    } else {
      Obj* unreached = object;
      object = object->next;

      // if there's a previous element, point it to the next object
      if (previous != NULL) {
        // skips over unreached by pointing previous to current's next
        previous->next = object;
      } else {
        // first element in list, reset top of VM.object stack
        vm.objects = object;
      }

      freeObject(unreached);
    }
  }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("--- gc begin\n");
#endif

  markRoots();
  traceReferences();
  tableRemoveWhite(&vm.strings);
  sweep();

#ifdef DEBUG_LOG_GC
  printf("--- gc end\n");
#endif
}
