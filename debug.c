#include <stdio.h>

#include "debug.h"

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < chunk->count;) {
    // since instructions in the VM can vary in size, let the disassembly tell us the next offset
    offset = disassembleInstruction(chunk, offset);
  }
}

static int simpleInstruction(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
  // the opcode is at the offset, so the index in the constant pool is the NEXT byte, so we add 1 to offset
  uint8_t constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);
  // this gets the name of the constant from the values pool of the chunk
  printValue(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 2; // offset is OP_CONSTANT, offset + 1 is index of constant in constant pool, offset + 2 is next instruction
}

int disassembleInstruction(Chunk* chunk,  int offset) {
  printf("%04d ", offset);
  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) { // if the line number is the same as the last line number we saw
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
    case OP_RETURN:
      return simpleInstruction("OP_RETURN", offset);
    case OP_CONSTANT:
      return constantInstruction("OP_CONSTANT", chunk, offset);
    default:
      printf("Unknown opcode %d\n", instruction);
      return offset + 1;
  }
}