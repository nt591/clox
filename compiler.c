#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "vm.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

// this is just an ever increasing enum of ints
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . () []
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix; // a function that says how to parse a prefix expression for a given token (aka -1)
  ParseFn infix; // a function that says how to parse an infix for a given token (aka 2 +)
  Precedence precedence; // the precedence of the infix for a given token
} ParseRule;

typedef struct {
  Token name; // identifier lexeme compares to local name
  int depth;
} Local;

// we want to basically have scopes for our functions
// if we're inside a function, we're in TYPE_FUNCTION
// if not, we'll pretend the global scope is a script, or TYPE_SCRIPT
typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
  struct Compiler* enclosing; // every compiler knows which function created it
  ObjFunction* function;
  FunctionType type;
  Local locals[UINT8_COUNT]; // just keep my local variables in the order of declaration
  int localCount; // how many local variables are in scope
  int scopeDepth; // level of nesting for our scopes
} Compiler;

Parser parser;

Compiler* current = NULL; // global parser so we don't need to pass a pointer through all our functions

Chunk* compilingChunk;

static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
  // once we see an error, set panic mode so we don't consume more error tokens
  if (parser.panicMode) return;
  parser.panicMode = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  }  else if (token->type == TOKEN_ERROR) {
    // nothing
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

static void advance() {
  // initialize compiler by reading off errors until we find a non-error token to start with
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message)  {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) {
  return parser.current.type == type;
}

static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  // when we have a byte for OpCode and a byte for operand
  // aka PUSH 2
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  // how far do we jump back?
  // we need to see where we are now, and where we started the loop
  // the difference is how far back we need to jump back
  // add two for the OP_LOOP operands
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  // buffer 2**16 bytes so we can backpatch later
  emitByte(0xff); // 2**8
  emitByte(0xff); // 2**8
  return currentChunk()->count - 2; // the offset of the instruction we emitted
}

static void emitReturn() {
  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  // add constant to pool, return value (which is index)
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Value value) {
  // add constant instruction and value to chunk
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
  // how far do we know how to jump?
  // the currentChunk()->count is where we are in bytecode for the statement (right after the then statement)
  // offset is where in bytecode the OP_JUMP_IF_FALSE / OP_JUMP instruction is
  // subtract two for length of OP_JUMP_IF_FALSE / OP_JUMP
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  // replacing the buffers we set up with the actual operands
  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
  // when we initialize a compiler, capture the current compiler as enclosing
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;

  /*
    to quote:
    We can create functions at compile time because they contain only data available at compile time.
    The function’s code, name, and arity are all fixed.
  */
  compiler->function = newFunction();
  current = compiler;

  // when we init compiler, it comes right after we read the function declaration
  // which means the previous token is the name of the function
  if (type != TYPE_SCRIPT) {
    // copy string since function can outlive a string object in memory
    current->function->name = copyString(parser.previous.start, parser.previous.length);
  }

  // VM claims the top of the locals stack for itself
  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->name.start = "";
  local->name.length = 0;
}

static ObjFunction* endCompiler() {
  // each pass of the compiler can handle one expression
  // every expression ends with a return
  emitReturn();

  // our compiler creates a function, fills its chunks, and returns the function
  ObjFunction* function = current->function;

  #ifndef DEBUG_PRINT_CODE
    if (!parser.hadError) {
      disassembleChunk(
        currentChunk(),
        function->name != NULL ? function->name->chars : "<script>");
    }
  #endif

  // when we end a compiler, go "back" a scope and reset current to the current's enclosing
  current = current->enclosing;
  return function;
}

static void beginScope() {
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  // time to pop off variables off the VM stack
  // as long as there are locals AND
  // as long as the back of the stack has locals with a scope depth higher than current depth
  // POP
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth >
            current->scopeDepth) {
    emitByte(OP_POP);
    current->localCount--;
  }
}

static void expression();
static void statement();
static void declaration();
static void funDeclaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence) {
  // read the next token
  // get the rule for it
  // if there's no rule, we have an error
  // run the rule
  // the first rule is always a prefix, even if its just an open paren
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  // now check if the precendence of the next token is less than what we saw
  // if it is, we consume it as infix
  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
    expression();
  }
}

static uint8_t identifierConstant(Token* name) {
  // we don't want to pass around the string representing a variable so
  // get the index from the constant pool and use that instead for our bytecode operand
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Cannot read local variable in its own initializer.");
      }
      return i;
    }
  }

  return -1;
}

static void addLocal(Token name) {
  // indexes are represented by one byte, or 2^8 valid indexes
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  // creating a local involves incrementing the count of locals and getting the next one in the list
}

static void declareVariable() {
  // globals are implicitly declared and latebound, so we dont need to declare early
  if (current->scopeDepth == 0) return;

  Token* name = &parser.previous;
  // we want to make sure that when a variable is declared, it hasnt been declared in the same scope already
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Variable with this name already declared in this scope.");
    }
  }

  addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  // if we're in some nested scope, it's a local variable
  // at runtime, we dont need to persist in the global table
  if (current->scopeDepth > 0) return 0;
  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  // if a top level function is declared, do nothing since no local variable
  if (current->scopeDepth == 0) return;

  // the last local added should have its depth set to the current depth
  current->locals[current->localCount - 1].depth =
      current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  /*
    Wait, what? Yup. That’s it. There is no code to create a local variable at runtime.
    Think about what state the VM is in.
    It has already executed the code for the variable’s initializer
    (or the implicit nil if the user omitted an initializer),
    and that value is sitting right on top of the stack as the only remaining temporary.
    We also know that new locals are allocated at the top of the stack… right where that value already is.
    Thus, there’s nothing to do.
    The temporary simply becomes the local variable. It doesn’t get much more efficient than that.
  */
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }
  emitBytes(OP_DEFINE_GLOBAL, global);
}

static void and_(bool canAssign) {
  // (1>0) and (2 > 1)
  // at this point, (1>0) is already compiled
  // so we look at top of stack and see, do we need to jump?
  // if false, jump pase parsePrecendence
  // else, we're good so pop and parse
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
}

static void binary(bool canAssign) {
  // get the last token type
  TokenType operatorType = parser.previous.type;

  // compile the right side
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  // emit the operator instruction
  switch (operatorType) {
    case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
    case TOKEN_GREATER: emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break; // greater or equal is same as not less than
    case TOKEN_LESS: emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS: emitByte(OP_ADD); break;
    case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
    default:
      return;
  }
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    default:
      return;
  }
}

static void parsePrecendence(Precedence precedence) {}

static void grouping(bool canAssign) {
  // when we see an open paren, consume an expression and get a close paren
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
  // string to double, get the string value of the number from the parser
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  // (1 > 0) or (1 < 0)
  // at this time, leftside is compiled and top of stack
  // since it's true, we can skip everything else
  // so we jump all the way to the else
  // if it IS false, we pop that value away and check the right side
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool canAssign) {
  emitConstant(
    OBJ_VAL(
      copyString(
        // clip preceding and trailing quotation marks
        parser.previous.start + 1,
        parser.previous.length - 2
      )
    )
  );
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }
  // if we see an equals sign, everything previous is an assignment
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
  // what's the last token we saw?
  TokenType operatorType = parser.previous.type;

  // compile the operand and push onto stack
  parsePrecedence(PREC_UNARY);

  // emit the VM instruction we need for the operator
  switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default:
      return;
  }
}

ParseRule rules[] = {
  { grouping, NULL,    PREC_NONE },       // TOKEN_LEFT_PAREN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RIGHT_PAREN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_LEFT_BRACE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RIGHT_BRACE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_COMMA
  { NULL,     NULL,    PREC_NONE },       // TOKEN_DOT
  { unary,    binary,  PREC_TERM },       // TOKEN_MINUS
  { NULL,     binary,  PREC_TERM },       // TOKEN_PLUS
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SEMICOLON
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_SLASH
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_STAR
  { unary,    NULL,    PREC_NONE },       // TOKEN_BANG
  { NULL,     binary,  PREC_EQUALITY },   // TOKEN_BANG_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_EQUAL
  { NULL,     binary,  PREC_EQUALITY },   // TOKEN_EQUAL_EQUAL
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_GREATER
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_GREATER_EQUAL
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_LESS
  { NULL,     binary,  PREC_COMPARISON }, // TOKEN_LESS_EQUAL
  { variable, NULL,    PREC_NONE },       // TOKEN_IDENTIFIER
  { string,   NULL,    PREC_NONE },       // TOKEN_STRING
  { number,   NULL,    PREC_NONE },       // TOKEN_NUMBER
  { NULL,     and_,    PREC_AND },        // TOKEN_AND
  { NULL,     NULL,    PREC_NONE },       // TOKEN_CLASS
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ELSE
  { literal,  NULL,    PREC_NONE },       // TOKEN_FALSE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FOR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FUN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_IF
  { literal,  NULL,    PREC_NONE },       // TOKEN_NIL
  { NULL,     or_,     PREC_OR },         // TOKEN_OR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_PRINT
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RETURN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SUPER
  { NULL,     NULL,    PREC_NONE },       // TOKEN_THIS
  { literal,  NULL,    PREC_NONE },       // TOKEN_TRUE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_VAR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_WHILE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ERROR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_EOF
};

static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

static void varDeclaration() {
  // if we saw the keyword var, the next value should be the variable name
  uint8_t global = parseVariable("Expect variable name.");

  // then if we see an equal sign, it's assignment
  // else, we just define it as nil, e.g
  // var x = 123;
  // var y;

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  // global is the index in the constant pool for the VM to look up
  defineVariable(global);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

// when the parser hits an error (some sort of unexpected token), we panic
// when we panic, we enter synchronize mode
// synchronize will reset panic, then (for declarations)
// just throw away tokens until we just saw a semicolon, indicating we get past the erroneous expression
// or we currently see the start of a new statement

//We recognize the boundary by looking for a preceding token that can end a statement, like a semicolon.
// Or we’ll look for a subsequent token that begins a statement, usually one of the control flow or declaration keywords.

static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) return;

    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;

      default:
        // Do nothing.
        ;
    }

    advance();
  }
}

static void declaration() {
  if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode) synchronize();
}

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  // each function gets its own inner compiler to generate it
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope();

  // parameters
  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Cannot have more than 255 parameters");
      }

      uint8_t paramConstant = parseVariable("Expect parameter name");
      defineVariable(paramConstant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

  // body
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  // create function object
  ObjFunction* function = endCompiler();
  emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
}

static void funDeclaration() {
  // if we see keyword fun, next token must be function name to declare
  uint8_t global = parseVariable("Expect function name.");
  markInitialized();

  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_POP);
}

static void ifStatement() {
  // gotta see a left paren, an expression, a right paren
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  // if the condition is false, how far away is the bytecode for the false branch?
  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();

  // we also want to make sure if condition was truthy, we jump over the else statement
  // see https://craftinginterpreters.com/image/jumping-back-and-forth/if-else.png
  // https://craftinginterpreters.com/image/jumping-back-and-forth/full-if-else.png
  // tldr, in the true case our bytecode says "is true, do THEN clause, then elseJump will jump over the else statement"
  // otherwise "is false, jump over then clause, do else clause"

  // [ CONDITION, JUMP_TO_POP_AND_ELSE_IF_CONDITION_FALSE, OP_POP, DO_THE_TRUE_THING,
  //  JUMP_OVER_ELSE_CLAUSE, OP_POP, ELSE_CLAUSE, WHAT_HAPPENS_AFTER_IF_STATEMENT...]
  int elseJump = emitJump(OP_JUMP);
  patchJump(thenJump);
  emitByte(OP_POP);
  // we also need to compile the else statement
  if (match(TOKEN_ELSE)) statement();
  patchJump(elseJump);
}

static void whileStatement() {
  int loopStart = currentChunk()->count; // where in the bytecode does this loop start;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after while.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  // set up my jump over the logic if it's false
  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP); // throw away the true evaluation from the condition
  statement();

  // since we know at this point, condition was true, do the statement and jump back to the top
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP); // throw away the false evaluation from the condition
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after for.");

  // since variable initialization is optional, we need to check the first clauses
  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  // now we have the optional exit condition, e.g x < 10
  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    // we have an exit condition since there's a non-semicolon
    // evaluate the condition, look for the next semicolon, then check if truthy and set jump

    expression();
    consume(TOKEN_SEMICOLON, "Expect ';'.");

    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // get rid of the condition if true
  }


  // now, optional incrementation clause for our optional variable
  if (!match(TOKEN_RIGHT_PAREN)) {
    //https://craftinginterpreters.com/image/jumping-back-and-forth/for.png
    // we're inside a incrementer
    // we jump over the increment, evaluate the body, come back to increment, then re-do the loop
    int bodyJump = emitJump(OP_JUMP);

    int incrementStart = currentChunk()->count;
    expression(); // the increment, e.g x++;
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Condition if false
  }
  endScope();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  }  else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

ObjFunction* compile(const char* source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  // as long as it's not the end of a file, get the next declaration
  // a declaration is a variable assignment or any statement
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  ObjFunction* function = endCompiler();
  return parser.hadError ? NULL : function;
}