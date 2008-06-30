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

typedef struct {
  int arity;
  void* proc;
} Lambda;

typedef Data* (*proc0)();
typedef Data* (*proc1)(Data*);
typedef Data* (*proc2)(Data*, Data*);
typedef Data* (*proc3)(Data*, Data*, Data*);
typedef Data* (*proc4)(Data*, Data*, Data*, Data*);
typedef Data* (*proc5)(Data*, Data*, Data*, Data*, Data*);
typedef Data* (*proc6)(Data*, Data*, Data*, Data*, Data*, Data*);
typedef Data* (*proc7)(Data*, Data*, Data*, Data*, Data*, Data*, Data*);
typedef Data* (*proc8)(Data*, Data*, Data*, Data*, Data*, Data*, Data*, Data*);
typedef Data* (*proc9)(Data*, Data*, Data*, Data*, Data*, Data*, Data*, Data*, Data*);

#define CONS(d) ((Cons*)(d->data))
#define CHARP(d) ((char*)(d->data))
#define INT(d) ((int)(d->data))
#define LAMBDA(d) ((Lambda*)(d->data))
#define PROC(d) ((void*)(LAMBDA(d)->proc))

#define CHECK(b) assert(b)
#define CHECK_IS_CONS(d) CHECK(d && d->type == T_CONS)
#define CHECK_IS_INT(d) CHECK(d && d->type == T_INT)
#define CHECK_IS_SYMBOL(d) CHECK(d && d->type == T_SYMBOL)
#define CHECK_IS_LAMBDA(d, a) CHECK(d && d->type == T_LAMBDA && LAMBDA(d)->arity == a)

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

Data* add(Data* d1, Data* d2) {
  CHECK_IS_INT(d1);
  CHECK_IS_INT(d2);

  int i = INT(d1) + INT(d2);
  Data* result = malloc(sizeof(Data));
  result->type = T_INT;
  result->data = (void*)i;
  return result;
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

Data* call0(Data* d) {
  CHECK_IS_LAMBDA(d, 0);
  return ((proc0)(PROC(d)))();
}

Data* call1(Data* d, Data* p1) {
  CHECK_IS_LAMBDA(d, 1);
  return ((proc1)(PROC(d)))(p1);
}

Data* call2(Data* d, Data* p1, Data* p2) {
  CHECK_IS_LAMBDA(d, 2);
  return ((proc2)(PROC(d)))(p1, p2);
}

Data* call3(Data* d, Data* p1, Data* p2, Data* p3) {
  CHECK_IS_LAMBDA(d, 3);
  return ((proc3)(PROC(d)))(p1, p2, p3);
}

Data* call4(Data* d, Data* p1, Data* p2, Data* p3, Data* p4) {
  CHECK_IS_LAMBDA(d, 4);
  return ((proc4)(PROC(d)))(p1, p2, p3, p4);
}

Data* call5(Data* d, Data* p1, Data* p2, Data* p3, Data* p4, Data* p5) {
  CHECK_IS_LAMBDA(d, 5);
  return ((proc5)(PROC(d)))(p1, p2, p3, p4, p5);
}

Data* call6(Data* d, Data* p1, Data* p2, Data* p3, Data* p4, Data* p5, Data* p6) {
  CHECK_IS_LAMBDA(d, 6);
  return ((proc6)(PROC(d)))(p1, p2, p3, p4, p5, p6);
}

Data* call7(Data* d, Data* p1, Data* p2, Data* p3, Data* p4, Data* p5, Data* p6, Data* p7) {
  CHECK_IS_LAMBDA(d, 7);
  return ((proc7)(PROC(d)))(p1, p2, p3, p4, p5, p6, p7);
}

Data* call8(Data* d, Data* p1, Data* p2, Data* p3, Data* p4, Data* p5, Data* p6, Data* p7,
	    Data* p8) {
  CHECK_IS_LAMBDA(d, 8);
  return ((proc8)(PROC(d)))(p1, p2, p3, p4, p5, p6, p7, p8);
}

Data* call9(Data* d, Data* p1, Data* p2, Data* p3, Data* p4, Data* p5, Data* p6, Data* p7,
	    Data* p8, Data* p9) {
  CHECK_IS_LAMBDA(d, 9);
  return ((proc9)(PROC(d)))(p1, p2, p3, p4, p5, p6, p7, p8, p9);
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
