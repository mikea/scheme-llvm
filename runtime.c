#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "runtime_shared.h"

typedef struct {
  void* data;
  char type;
} Data;

typedef struct {
  Data* car;
  Data* cdr;
} Cons;

#define CHECK(b) assert(b)
#define CHECK_IS_CONS(d) CHECK(d->type == T_CONS)
#define CHECK_IS_INT(d) CHECK(d->type == T_INT)
#define CHECK_IS_SYMBOL(d) CHECK(d->type == T_SYMBOL)

#define CONS(d) ((Cons*)(d->data))
#define CHARP(d) ((char*)(d->data))
#define INT(d) ((int)(d->data))

Data* car(Data* d) {
  if (!d) {
    fprintf(stderr, "ERROR: calling (car ())\n");
    exit(2);
  }
  CHECK_IS_CONS(d);
  return CONS(d)->car;
}

Data* cdr(Data* d) {
  if (!d) {
    fprintf(stderr, "ERROR: calling (cdr ())\n");
    exit(2);
  }
  CHECK_IS_CONS(d);
  return CONS(d)->cdr;
}

Data* display(Data* d);

void display_int(Data* d) {
  CHECK_IS_INT(d);
  printf("%d", INT(d));
}

void display_symbol(Data* d) {
  CHECK_IS_SYMBOL(d);
  printf("%s", CHARP(d));
}

void display_cons(Data* d) {
  CHECK_IS_CONS(d);
  printf("(");
  display(car(d));
  Data* t = cdr(d);
  while (t) {
    if (t->type == T_CONS) {
      printf(" ");
      display(car(t));
      t = cdr(t);
    }
    else {
      printf(" . ");
      display(t);
      break;
    }
  }
  printf(")");
}

Data* display(Data* d) {
  if (!d) {
    printf("()");
    return 0;
  }
  switch (d->type) {
  case T_INT : 
    display_int(d);
    break;
  case T_SYMBOL :
    display_symbol(d);
    break;
  case T_CONS :
    display_cons(d);
    break;
  default : 
    fprintf(stderr, "Unknown type: %d\n", d->type);
    assert(0);
  }
  return 0;
}

Data* symbols;
int symbols_size;
int symbols_count;

void init_symbols() {
  symbols_size = 100;
  symbols = malloc(sizeof(Data) * symbols_size);
  memset(symbols, 0, sizeof(Data) * symbols_size);
  symbols_count = 0;
}

Data* string_to_symbol(char* str) {
  int i;
  for (i = 0; i < symbols_count; i++) {
    if (!strcmp(str, (char*)symbols[i].data)) {
      return &symbols[i];
    }
  }

  // not found, should add new one

  if (symbols_count == symbols_size) {
    // reallocate
    Data* old = symbols;
    symbols = malloc(sizeof(Data) * symbols_size * 2);
    memset(symbols, 0, sizeof(Data) * symbols_size * 2);
    memcpy(symbols, old, sizeof(Data) * symbols_size);
    symbols_size *= 2;
  }

  Data *d = &symbols[symbols_count];
  symbols_count++;

  d->type = T_SYMBOL;
  int len = strlen(str);
  d->data = malloc(len + 1);
  memcpy(d->data, str, len + 1);
  return d;
}

void scheme_main(void);
void scheme_init(void);

int main(int argc, char** argv) {
  init_symbols();
  scheme_init();
  scheme_main();
  return 0;
}
