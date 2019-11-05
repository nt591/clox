#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// for now, we'll just represent double-precision floats
typedef double Value;

// our constant pool, an array of values
// instructions will carry an index to a position in the pool rather than the constant itself
// this way we can keep our instruction size small even for big constants (like giant strings)
typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif