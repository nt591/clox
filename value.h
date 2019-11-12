#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// forward declaration of types in object.h
typedef struct sObj Obj;
typedef struct sObjString ObjString;

// defining an enum that represents the types our VM knows about
typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  VAL_OBJ, // anything that's variable sized, so we store it on the heap
} ValueType;

// https://www.tutorialspoint.com/cprogramming/c_unions.htm
// a Value can be one of the ValueType, and based on that, the value is in the union
// unions are stored in the same space in memory and that size is the size of the largest type
// in this case, bool = 1 byte, double = 8 bytes, so union is 8 bytes
// https://craftinginterpreters.com/types-of-values.html
// this is a tagged union
typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj* obj; // Values of obj have a pointer to a location on the heap
  } as;
} Value;

// need macros to type check our Value objects so we can get the data out
// aka, don't wanna call value.as.double on BOOL_VAL(true)
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

// getting the value data out of a Value object
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)

#define BOOL_VAL(value) ((Value){ VAL_BOOL, {.boolean = value} })
#define NIL_VAL ((Value){ VAL_NIL, {.number = 0} })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .number = value } })
#define OBJ_VAL(object)  ((Value){ VAL_OBJ, { .obj = (Obj*)object } })

// our constant pool, an array of values
// instructions will carry an index to a position in the pool rather than the constant itself
// this way we can keep our instruction size small even for big constants (like giant strings)
typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif